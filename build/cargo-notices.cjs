const { execFileSync } = require('node:child_process');
const fs = require('node:fs');
const path = require('node:path');

const [manifest, platform, destination] = process.argv.slice(2);
const metadata = JSON.parse(execFileSync('cargo', [
  'metadata', '--locked', '--offline', '--format-version', '1',
  '--filter-platform', platform, '--manifest-path', manifest
], { encoding: 'utf8', maxBuffer: 32 * 1024 * 1024 }));

const nodes = new Map(metadata.resolve.nodes.map(node => [node.id, node]));
const resolved = new Set();
function visit(id) {
  if (resolved.has(id)) return;
  resolved.add(id);
  for (const dependency of nodes.get(id).deps) {
    if (dependency.dep_kinds.some(edge => edge.kind !== 'dev')) visit(dependency.pkg);
  }
}
visit(metadata.resolve.root);

// These crate archives omit their workspace-level notices. Match the published
// source revision so an update with different terms cannot reuse an old notice.
// objc2's full license texts were published in ee9a7ada2131f5944b8750428e265c15632f2a19.
const workspaceNotices = {
  'https://github.com/eiz/accessibility': ['accessibility', [
    '63a4a9c4ecdc143bed1d65b989aa18bbc5fd8129'
  ]],
  'https://github.com/madsmtm/objc2': ['objc2', [
    'b4167b582b2f75f9a1be75495c41b765344fd03c', '8852b424193ca41602281b3d7540d7c8ed51e49a',
    'd7d2fa23ceaa5e6096c923b081040e5d81b3b9df', '7b1abfd750a2cacaea71d6a56ecfb83cb7de560b',
    '8d214f5477365ffcbcbb7de058c86ed9a518efb7'
  ]]
};

fs.mkdirSync(destination);
const inventory = [];
for (const pkg of metadata.packages) {
  if (!pkg.source || !resolved.has(pkg.id)) continue;
  const directory = path.dirname(pkg.manifest_path);
  const target = path.join(destination, pkg.name + '-' + pkg.version);
  fs.mkdirSync(target);
  const notices = fs.readdirSync(directory).filter(name => /^(license|copying|notice)/i.test(name));
  if (pkg.license_file && !notices.includes(pkg.license_file)) notices.push(pkg.license_file);
  if (!notices.length) {
    const fallback = workspaceNotices[pkg.repository];
    const provenance = path.join(directory, '.cargo_vcs_info.json');
    if (!fallback || !fs.existsSync(provenance) ||
        !fallback[1].includes(JSON.parse(fs.readFileSync(provenance, 'utf8')).git.sha1)) {
      throw new Error('Dependency notice missing: ' + pkg.name + '-' + pkg.version);
    }
    const source = path.join(__dirname, 'package', 'cargo-notices', fallback[0]);
    for (const file of fs.readdirSync(source)) {
      if (/^LICENSE-(APACHE|ZLIB)/.test(file) && pkg.license === 'MIT') continue;
      fs.copyFileSync(path.join(source, file), path.join(target, file));
    }
  }
  for (const name of notices) {
    fs.cpSync(path.resolve(directory, name), path.join(target, path.basename(name)),
      { recursive: true, dereference: true });
  }
  const { name, version, license, repository } = pkg;
  inventory.push({ name, version, license, repository });
}
fs.writeFileSync(path.join(destination, 'inventory.json'), JSON.stringify(inventory, null, 2) + '\n');
