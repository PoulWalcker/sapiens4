#!/usr/bin/env python3
"""Stage the complete pinned upstream Codex package; never use a developer install."""
import argparse
import json
import pathlib
import shutil
import tarfile
import tempfile
import urllib.request
from package import digest, inventory, verify_inventory


def download(record, destination):
    with urllib.request.urlopen(record["url"], timeout=120) as source, destination.open("wb") as target:
        shutil.copyfileobj(source, target)
    if digest(destination) != record["sha256"]:
        raise ValueError("Pinned provider download checksum mismatch")
    if "size" in record and destination.stat().st_size != record["size"]:
        raise ValueError("Pinned provider download size mismatch")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--platform", required=True, choices=["darwin-arm64", "windows-x64"])
    parser.add_argument("--destination", required=True, type=pathlib.Path)
    parser.add_argument("--ensure", action="store_true", help="Verify an existing payload or acquire it when absent")
    parser.add_argument("--cache", type=pathlib.Path, help="Verified download cache (build time only)")
    args = parser.parse_args()
    lock = json.loads((pathlib.Path(__file__).resolve().parents[2] / "build/codex-provider-lock.json").read_text())
    asset = lock["platforms"][args.platform]
    destination = args.destination.resolve()
    if destination.exists():
        if not args.ensure:
            raise ValueError("Provider destination already exists; use --ensure to verify it")
        metadata = json.loads((destination / "sapiens-provider.json").read_text())
        expected = {"schema": 1, "version": lock["version"], "platform": args.platform,
                    "archiveSha256": asset["sha256"], "entrypoint": asset["entrypoint"]}
        if any(metadata.get(key) != value for key, value in expected.items()):
            raise ValueError("Existing provider does not match the pinned release; use a fresh destination")
        verify_inventory(destination, metadata["files"], {"sapiens-provider.json"})
        print(json.dumps({"version": lock["version"], "destination": str(destination), "verified": True}))
        return
    destination.parent.mkdir(parents=True, exist_ok=True)
    cache = args.cache or destination.parent / ".downloads"
    cache.mkdir(parents=True, exist_ok=True)
    archive = cache / (asset["sha256"] + ".tar.gz")
    if not archive.exists() or digest(archive) != asset["sha256"]:
        partial = archive.with_suffix(".downloading")
        download(asset, partial)
        partial.replace(archive)
    with tempfile.TemporaryDirectory(prefix=".codex-stage-", dir=destination.parent) as temporary:
        stage = pathlib.Path(temporary)
        unpacked = stage / "payload"
        unpacked.mkdir()
        with tarfile.open(archive, "r:gz") as tar:
            # Do not permit archive paths or links to escape the immutable payload.
            for entry in tar.getmembers():
                name = pathlib.PurePosixPath(entry.name)
                if name.is_absolute() or ".." in name.parts or "\\" in entry.name or ":" in entry.name or pathlib.PureWindowsPath(entry.name).drive or not (entry.isfile() or entry.isdir()):
                    raise ValueError("Unsafe provider archive entry")
            tar.extractall(unpacked)
        roots = list(unpacked.rglob("codex-package.json"))
        if len(roots) != 1:
            raise ValueError("Complete Codex package manifest missing or ambiguous")
        payload = roots[0].parent
        manifest = json.loads(roots[0].read_text())
        target = {"darwin-arm64": "aarch64-apple-darwin", "windows-x64": "x86_64-pc-windows-msvc"}[args.platform]
        if manifest.get("layoutVersion") != 1 or manifest.get("version") != lock["version"] or manifest.get("target") != target or manifest.get("variant") != "codex" or manifest.get("entrypoint") != asset["entrypoint"]:
            raise ValueError("Pinned provider package identity/layout mismatch")
        required = [asset["entrypoint"], "bin/codex-code-mode-host" + (".exe" if args.platform == "windows-x64" else ""), "codex-path/rg" + (".exe" if args.platform == "windows-x64" else "")]
        for key in ["entrypoint", "execServer", "resourcesDir", "pathDir"]:
            declared = manifest.get(key)
            if declared is None and key == "execServer":
                continue
            if not isinstance(declared, str) or not declared or "\\" in declared or ":" in declared or pathlib.PurePosixPath(declared).is_absolute() or ".." in pathlib.PurePosixPath(declared).parts:
                raise ValueError("Unsafe or missing declared provider path: " + key)
            if key in ["resourcesDir", "pathDir"]:
                if not (payload / declared).is_dir() or not any((payload / declared).rglob("*")):
                    raise ValueError("Missing provider resource directory: " + declared)
            else:
                required.append(declared)
        for path in required:
            if not (payload / path).is_file():
                raise ValueError("Incomplete provider package: " + path)
        for notice in lock["notices"]:
            download(notice, payload / notice["path"])
        files = inventory(payload, {"sapiens-provider.json"})
        metadata = {"schema": 1, "version": lock["version"], "upstreamCommit": lock["upstreamCommit"], "platform": args.platform, "entrypoint": asset["entrypoint"], "archiveSha256": asset["sha256"], "upstreamPackage": manifest, "files": files}
        (payload / "sapiens-provider.json").write_text(json.dumps(metadata, indent=2) + "\n")
        payload.rename(destination)
    print(json.dumps({"version": lock["version"], "platform": args.platform, "files": len(files), "destination": str(destination)}))


if __name__ == "__main__":
    main()
