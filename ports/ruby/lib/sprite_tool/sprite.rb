# frozen_string_literal: true

require "open3"
require "shellwords"

module SpriteTool
  # Wrapper around the sprite CLI, providing exec, file transfer,
  # checkpoint, and lifecycle methods.
  class Sprite
    attr_reader :name, :dry_run

    def initialize(name, dry_run: false)
      @name = name
      @dry_run = dry_run
    end

    # Run `sprite exec -s <sprite> bash -c "<cmd>"` and return stdout.
    # Raises on non-zero exit.
    def exec(cmd)
      if @dry_run
        puts "  [dry-run] sprite exec -s #{@name} bash -c #{cmd.shellescape}"
        return ""
      end
      out, status = Open3.capture2("sprite", "exec", "-s", @name, "bash", "-c", cmd)
      unless status.success?
        raise "sprite exec failed (exit #{status.exitstatus}): #{cmd}\n#{out}"
      end
      out.strip
    end

    # Run `sprite exec -s <sprite> bash -c "<cmd>"` piping stdin_data through stdin.
    def exec_with_stdin(cmd, stdin_data)
      if @dry_run
        puts "  [dry-run] sprite exec -s #{@name} bash -c #{cmd.shellescape} < (stdin)"
        return ""
      end
      out, status = Open3.capture2(
        "sprite", "exec", "-s", @name, "bash", "-c", cmd,
        stdin_data: stdin_data
      )
      unless status.success?
        raise "sprite exec with stdin failed (exit #{status.exitstatus}): #{cmd}\n#{out}"
      end
      out.strip
    end

    # Run `sprite exec -s <sprite> bash -c "<cmd>"` with stdin/stdout/stderr
    # connected to the terminal (interactive).
    def exec_interactive(cmd)
      if @dry_run
        puts "  [dry-run] sprite exec -s #{@name} bash -c #{cmd.shellescape}"
        return true
      end
      system("sprite", "exec", "-s", @name, "bash", "-c", cmd)
    end

    # Run `sprite exec -s <sprite> <args...>` with stdout piped to a writer,
    # stderr to terminal. Returns [stdout_read_io, wait_thread].
    def exec_piped_stdout(*args)
      full_args = ["sprite", "exec", "-s", @name] + args
      if @dry_run
        puts "  [dry-run] #{full_args.join(" ")}"
        return [StringIO.new(""), nil]
      end
      stdin_r, stdin_w = IO.pipe
      stdin_w.close
      stdout_r, stdout_w = IO.pipe
      pid = spawn(*full_args, in: stdin_r, out: stdout_w, err: $stderr)
      stdin_r.close
      stdout_w.close
      wait_thread = Process.detach(pid)
      [stdout_r, wait_thread]
    end

    # Run `sprite exec -s <sprite> bash -c "<cmd>"`, piping local stdin to remote
    # and remote stdout to local stdout. Used for tar streaming.
    def exec_pipe_through(cmd, stdin_io: nil, stdout_io: nil)
      if @dry_run
        puts "  [dry-run] sprite exec -s #{@name} bash -c #{cmd.shellescape}"
        return true
      end

      opts = {}
      opts[:in] = stdin_io if stdin_io
      opts[:out] = stdout_io if stdout_io
      opts[:err] = $stderr

      system("sprite", "exec", "-s", @name, "bash", "-c", cmd, **opts)
    end

    # Push a local file to the sprite at dest path.
    def push_file(src, dest)
      if @dry_run
        puts "  [dry-run] push #{src} -> sprite:#{dest}"
        return
      end
      dest_dir = File.dirname(dest)
      self.exec("mkdir -p #{dest_dir.shellescape}")
      data = File.binread(src)
      exec_with_stdin("cat > #{dest.shellescape}", data)
    end

    # Push a local directory to the sprite at dest path via tar.
    def push_dir(src, dest)
      if @dry_run
        puts "  [dry-run] push dir #{src} -> sprite:#{dest}"
        return
      end
      self.exec("mkdir -p #{dest.shellescape}")
      parent = File.dirname(src)
      base = File.basename(src)
      dest_parent = File.dirname(dest)

      # Use Open3.popen3 to pipe tar through sprite exec
      tar_cmd = ["tar", "czf", "-", "-C", parent, base]
      remote_cmd = "tar xzf - -C #{dest_parent.shellescape}"
      sprite_cmd = ["sprite", "exec", "-s", @name, "bash", "-c", remote_cmd]

      Open3.pipeline(tar_cmd, sprite_cmd)
    end

    # Run a plain `sprite <args...>` command and return stdout.
    def run(*args)
      if @dry_run
        puts "  [dry-run] sprite #{args.join(" ")}"
        return ""
      end
      out, status = Open3.capture2("sprite", *args)
      unless status.success?
        raise "sprite #{args.join(" ")} failed (exit #{status.exitstatus})\n#{out}"
      end
      out.strip
    end

    # Run a plain `sprite <args...>` command with terminal I/O.
    def run_passthrough(*args)
      if @dry_run
        puts "  [dry-run] sprite #{args.join(" ")}"
        return true
      end
      system("sprite", *args)
    end

    # Check if the sprite CLI is installed; install if not.
    def self.ensure_cli(dry_run: false)
      return if system("which sprite > /dev/null 2>&1")

      puts "sprite CLI not found, installing..."
      if dry_run
        puts "  [dry-run] curl -fsSL https://sprites.dev/install.sh | sh"
        return
      end
      system("bash", "-c", "curl -fsSL https://sprites.dev/install.sh | sh")
      ENV["PATH"] = "#{ENV["HOME"]}/.local/bin:#{ENV["PATH"]}"
    end

    # Authenticate with sprite using token or interactive login.
    def self.auth(token, dry_run: false)
      if token && !token.empty?
        puts "Authenticating sprite with token..."
        unless dry_run
          system("sprite", "auth", "setup", "--token", token)
        end
      else
        puts "No SPRITE_TOKEN set. Running interactive login..."
        unless dry_run
          system("sprite", "login")
        end
      end
    end

    # Check if this sprite already exists.
    def exists?
      if @dry_run
        puts "  [dry-run] sprite ls (checking for #{@name})"
        return false
      end
      out, status = Open3.capture2("sprite", "ls")
      return false unless status.success?
      out.split("\n").any? { |line| line.include?(@name) }
    end

    # Create the sprite with `sprite create -skip-console <name>`.
    def create
      if @dry_run
        puts "  [dry-run] sprite create -skip-console #{@name}"
        return
      end
      system("sprite", "create", "-skip-console", @name)
    end

    # Create a checkpoint of the sprite.
    def checkpoint_create
      if @dry_run
        puts "  [dry-run] sprite checkpoint create -s #{@name}"
        return true
      end
      system("sprite", "checkpoint", "create", "-s", @name)
    end

    # Open a console to the sprite.
    def console
      if @dry_run
        puts "  [dry-run] sprite console -s #{@name}"
        return
      end
      system("sprite", "console", "-s", @name)
    end
  end
end
