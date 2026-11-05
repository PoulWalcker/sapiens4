require_relative 'desktop'
require 'sqlite3'
require 'yaml'

module RoutineCrashRecovery
  def crash_click_visible(label)
    target = desktop.find(label)
    desktop.control('scroll-to', '--path', target.fetch('path'))
    desktop.click(label)
  end

  def crash_message(name, text)
    desktop.fill("Message #{name}", text)
    desktop.click('Send message')
    desktop.value("Message #{name}", '')
  end

  def crash_chief_request(text, actions)
    host.configure(messages: [{contains: text, node: 'Execution', rule: {actions: actions}}])
    crash_message('ChiefSapi', text)
    host.idle
  end

  def crash_group
    host.api('GET', "/api/groups/#{@crash_group_id}")
  end

  def crash_routine(id)
    host.state.fetch('routines').find { |row| row['id'] == id }
  end

  def crash_turns(id)
    host.state.fetch('turns').select { |row| row.dig('origin', 'routine') == id }
  end

  def crash_entries(path)
    File.exist?(path) ? File.readlines(path).map { |line| JSON.parse(line) } : []
  end

  def crash_occurrence(id)
    SQLite3::Database.open(File.join(host.root, 'corpora.sqlite3'), readonly: true) do |db|
      db.get_first_row('SELECT id,status,value FROM routine_occurrences WHERE id=?', [id])
    end
  end

  def crash_task(owner, id)
    YAML.safe_load(host.api('GET', "/api/agents/#{owner}/tasks")).fetch('tasks')
      .find { |row| row['workloadId'] == id }
  end

  def crash_select_personal
    crash_select_scope('Sapis')
    desktop.see('Open ChiefSapi')
    desktop.click('Open CrashPersonal')
    desktop.see('Message CrashPersonal')
  end

  def crash_select_group
    crash_select_scope('Groups')
    desktop.click(/Open Crash Group/)
    desktop.see('Message Crash Group')
  end

  def crash_select_scope(label)
    desktop.click(label)
    Contract.wait(description: "#{label} roster selected before locating a row") do
      desktop.find(label).fetch('value', 0).to_s.to_f == 1.0
    end
  end

  def crash_pause(name, id)
    desktop.click('Work menu')
    desktop.click('Automation menu')
    desktop.click('Routines')
    desktop.click('Pause routine')
    desktop.value("Message #{name}", "Pause Scheduled Routine\n@#{id}")
    desktop.click('Send message')
    Contract.wait(description: "#{name} schedule paused while its call is still running") { crash_routine(id)['paused'] }
  end

  def crash_save_policy(name, owner, prompt, node)
    policy = File.join(host.root, "#{node}-crash-policy.json")
    receipt = policy + '.receipt'
    host.configure(messages: [{contains: 'Configure the missed-deadline policy', node: node, rule: {
      actions: [{gate: "#{node}-policy-ready"}, {controlFrom: policy, receipt: receipt}]}}])
    crash_message(name, "Add New Scheduled Routine:\nFrequency: every 10 minutes\nExecute: #{prompt}")
    routine = Contract.wait(description: "#{name} routine saved through its conversation") do
      host.state.fetch('routines').find { |row| row['owner'] == owner }
    end
    Contract.atomic_json(policy, {op: 'routine_policy', id: routine.fetch('id'),
      expectedPrompt: routine.fetch('prompt'), expectedSelection: routine.fetch('policySelection'), missedPolicy: 'skip_missed'})
    host.release("#{node}-policy-ready")
    Contract.wait(description: "#{name} model saves skip_missed with fresh host-control credentials") do
      File.exist?(receipt) && crash_routine(routine.fetch('id'))['policySelection'] == {'kind' => 'selected', 'policy' => 'skip_missed'}
    end
    expect(JSON.parse(File.read(receipt))).to include('saved' => true)
    host.idle
    routine
  end

  def crash_start_minute_schedule(name, routine)
    desktop.click('Work menu')
    desktop.click('Automation menu')
    desktop.click('Routines')
    desktop.click('Edit routine')
    crash_message(name, "Edit @#{routine.fetch('id')} ( #{routine.fetch('title')} )\n" \
      "Edit Frequency (prev every 10 minutes): new every 1 minutes\nEdit Execution Prompt as: #{routine.fetch('prompt')}")
    Contract.wait(description: "#{name} minute schedule starts after both effect gates are configured") do
      crash_routine(routine.fetch('id'))['minutes'] == 1
    end
  end

  def crash_process_identity(pid)
    # Keep the fixture's Unicode workspace path literal for ownership checks.
    output, error, status = Open3.capture3({'LC_ALL' => 'en_US.UTF-8'}, 'ps', '-ww', '-p', pid.to_s, '-o', 'lstart=', '-o', 'command=')
    return nil if status.exitstatus == 1 && output.empty?
    raise "Cannot inspect fixture provider #{pid}: #{error}" unless status.success?
    output.strip
  end

  def crash_own_provider(call)
    workspace = File.realpath(call.fetch('cwd'))
    raise 'Provider workspace is outside this fixture' unless workspace.start_with?(File.realpath(host.root) + File::SEPARATOR)
    identity = crash_process_identity(call.fetch('pid'))
    raise 'Gated fixture provider is no longer alive' unless identity
    raise 'Provider command does not belong to this fixture' unless identity.include?(File.join(host.root, 'codex'))
    @crash_owned_providers ||= []
    @crash_owned_providers << {pid: call.fetch('pid'), identity: identity, started: call.fetch('started'), cwd: workspace}
  end

  def crash_cleanup_providers
    Array(@crash_owned_providers).each do |owned|
      identity = crash_process_identity(owned.fetch(:pid))
      next unless identity
      raise "Fixture provider PID #{owned.fetch(:pid)} changed identity; refusing to signal it" unless identity == owned.fetch(:identity)
      Process.kill('KILL', owned.fetch(:pid))
      Contract.wait(description: "owned orphan provider #{owned.fetch(:pid)} exits") do
        current = crash_process_identity(owned.fetch(:pid))
        !current || current.include?('<defunct>')
      end
    rescue Errno::ESRCH
      next
    end
    @crash_owned_providers = []
  end
end
