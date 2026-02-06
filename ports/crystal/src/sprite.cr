module SpriteTool
  # Wrapper around the sprite CLI, providing exec, file transfer,
  # checkpoint, and lifecycle methods.
  class Sprite
    getter name : String
    getter dry_run : Bool

    def initialize(@name : String, @dry_run : Bool = false)
    end

    # Run `sprite exec -s <sprite> bash -c "<cmd>"` and return stdout.
    # Raises on non-zero exit.
    def exec(cmd : String) : String
      if @dry_run
        puts "  [dry-run] sprite exec -s #{@name} bash -c #{cmd.inspect}"
        return ""
      end
      output = IO::Memory.new
      error = IO::Memory.new
      status = Process.run(
        "sprite",
        ["exec", "-s", @name, "bash", "-c", cmd],
        output: output,
        error: error
      )
      unless status.success?
        raise "sprite exec failed (exit #{status.exit_code}): #{cmd}\n#{output.to_s}#{error.to_s}"
      end
      output.to_s.strip
    end

    # Run `sprite exec -s <sprite> bash -c "<cmd>"` piping stdin_data through stdin.
    def exec_with_stdin(cmd : String, stdin_data : String | Bytes) : String
      if @dry_run
        puts "  [dry-run] sprite exec -s #{@name} bash -c #{cmd.inspect} < (stdin)"
        return ""
      end
      output = IO::Memory.new
      error = IO::Memory.new
      input = IO::Memory.new
      case stdin_data
      when String
        input.print(stdin_data)
      when Bytes
        input.write(stdin_data)
      end
      input.rewind
      status = Process.run(
        "sprite",
        ["exec", "-s", @name, "bash", "-c", cmd],
        input: input,
        output: output,
        error: error
      )
      unless status.success?
        raise "sprite exec with stdin failed (exit #{status.exit_code}): #{cmd}\n#{output.to_s}#{error.to_s}"
      end
      output.to_s.strip
    end

    # Run `sprite exec -s <sprite> bash -c "<cmd>"` with stdin/stdout/stderr
    # connected to the terminal (interactive).
    def exec_interactive(cmd : String) : Bool
      if @dry_run
        puts "  [dry-run] sprite exec -s #{@name} bash -c #{cmd.inspect}"
        return true
      end
      status = Process.run(
        "sprite",
        ["exec", "-s", @name, "bash", "-c", cmd],
        input: Process::Redirect::Inherit,
        output: Process::Redirect::Inherit,
        error: Process::Redirect::Inherit
      )
      status.success?
    end

    # Push a local file to the sprite at dest path.
    def push_file(src : String, dest : String)
      if @dry_run
        puts "  [dry-run] push #{src} -> sprite:#{dest}"
        return
      end
      dest_dir = File.dirname(dest)
      self.exec("mkdir -p '#{dest_dir}'")
      data = File.read(src)
      exec_with_stdin("cat > '#{dest}'", data)
    end

    # Push a local directory to the sprite at dest path via tar.
    def push_dir(src : String, dest : String)
      if @dry_run
        puts "  [dry-run] push dir #{src} -> sprite:#{dest}"
        return
      end
      self.exec("mkdir -p '#{dest}'")
      parent = File.dirname(src)
      base = File.basename(src)
      dest_parent = File.dirname(dest)

      # Pipe tar through sprite exec using Process with pipes
      tar_process = Process.new(
        "tar",
        ["czf", "-", "-C", parent, base],
        output: Process::Redirect::Pipe,
        error: Process::Redirect::Inherit
      )

      remote_cmd = "tar xzf - -C '#{dest_parent}'"
      sprite_process = Process.new(
        "sprite",
        ["exec", "-s", @name, "bash", "-c", remote_cmd],
        input: Process::Redirect::Pipe,
        output: Process::Redirect::Inherit,
        error: Process::Redirect::Inherit
      )

      # Copy tar stdout to sprite stdin
      IO.copy(tar_process.output, sprite_process.input)
      sprite_process.input.close
      tar_process.output.close

      tar_process.wait
      sprite_process.wait
    end

    # Run a plain `sprite <args...>` command and return stdout.
    def run(args : Array(String)) : String
      if @dry_run
        puts "  [dry-run] sprite #{args.join(" ")}"
        return ""
      end
      output = IO::Memory.new
      error = IO::Memory.new
      status = Process.run("sprite", args, output: output, error: error)
      unless status.success?
        raise "sprite #{args.join(" ")} failed (exit #{status.exit_code})\n#{output.to_s}#{error.to_s}"
      end
      output.to_s.strip
    end

    # Run a plain `sprite <args...>` command with terminal I/O.
    def run_passthrough(args : Array(String)) : Bool
      if @dry_run
        puts "  [dry-run] sprite #{args.join(" ")}"
        return true
      end
      status = Process.run(
        "sprite", args,
        input: Process::Redirect::Inherit,
        output: Process::Redirect::Inherit,
        error: Process::Redirect::Inherit
      )
      status.success?
    end

    # Check if the sprite CLI is installed; install if not.
    def self.ensure_cli(dry_run : Bool = false)
      # Check if sprite is on PATH
      which_status = Process.run("which", ["sprite"], output: Process::Redirect::Close, error: Process::Redirect::Close)
      return if which_status.success?

      puts "sprite CLI not found, installing..."
      if dry_run
        puts "  [dry-run] curl -fsSL https://sprites.dev/install.sh | sh"
        return
      end
      Process.run(
        "bash", ["-c", "curl -fsSL https://sprites.dev/install.sh | sh"],
        input: Process::Redirect::Inherit,
        output: Process::Redirect::Inherit,
        error: Process::Redirect::Inherit
      )
      home = ENV["HOME"]? || "~"
      ENV["PATH"] = "#{home}/.local/bin:#{ENV["PATH"]? || ""}"
    end

    # Authenticate with sprite using token or interactive login.
    def self.auth(token : String, dry_run : Bool = false)
      if !token.empty?
        puts "Authenticating sprite with token..."
        unless dry_run
          Process.run(
            "sprite", ["auth", "setup", "--token", token],
            input: Process::Redirect::Inherit,
            output: Process::Redirect::Inherit,
            error: Process::Redirect::Inherit
          )
        end
      else
        puts "No SPRITE_TOKEN set. Running interactive login..."
        unless dry_run
          Process.run(
            "sprite", ["login"],
            input: Process::Redirect::Inherit,
            output: Process::Redirect::Inherit,
            error: Process::Redirect::Inherit
          )
        end
      end
    end

    # Check if this sprite already exists.
    def exists? : Bool
      if @dry_run
        puts "  [dry-run] sprite ls (checking for #{@name})"
        return false
      end
      output = IO::Memory.new
      status = Process.run("sprite", ["ls"], output: output, error: Process::Redirect::Close)
      return false unless status.success?
      output.to_s.split("\n").any? { |line| line.includes?(@name) }
    end

    # Create the sprite with `sprite create -skip-console <name>`.
    def create
      if @dry_run
        puts "  [dry-run] sprite create -skip-console #{@name}"
        return
      end
      Process.run(
        "sprite", ["create", "-skip-console", @name],
        input: Process::Redirect::Inherit,
        output: Process::Redirect::Inherit,
        error: Process::Redirect::Inherit
      )
    end

    # Create a checkpoint of the sprite.
    def checkpoint_create : Bool
      if @dry_run
        puts "  [dry-run] sprite checkpoint create -s #{@name}"
        return true
      end
      status = Process.run(
        "sprite", ["checkpoint", "create", "-s", @name],
        output: Process::Redirect::Inherit,
        error: Process::Redirect::Close
      )
      status.success?
    end

    # Open a console to the sprite.
    def console
      if @dry_run
        puts "  [dry-run] sprite console -s #{@name}"
        return
      end
      Process.run(
        "sprite", ["console", "-s", @name],
        input: Process::Redirect::Inherit,
        output: Process::Redirect::Inherit,
        error: Process::Redirect::Inherit
      )
    end
  end
end
