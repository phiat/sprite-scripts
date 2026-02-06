module SpriteTool
  module Push
    # Push a local file or directory to a sprite.
    #
    # local_path  - Path to a local file or directory.
    # remote_path - Destination path on the sprite.
    # sprite_name - Optional sprite name (uses default if nil/empty).
    def self.run(local_path : String, remote_path : String, sprite_name : String?)
      unless File.exists?(local_path)
        STDERR.puts "Error: #{local_path} does not exist"
        exit 1
      end

      sprite_args = [] of String
      if sprite_name && !sprite_name.empty?
        sprite_args.push("-s", sprite_name)
      end

      if File.directory?(local_path)
        puts "Pushing directory: #{local_path} -> #{remote_path}"

        parent = File.dirname(local_path)
        base = File.basename(local_path)

        # Pipe tar through sprite exec
        tar_process = Process.new(
          "tar",
          ["czf", "-", "-C", parent, base],
          output: Process::Redirect::Pipe,
          error: Process::Redirect::Inherit
        )

        sprite_cmd_args = ["exec"] + sprite_args + ["bash", "-c",
          "mkdir -p '#{remote_path}' && tar xzf - -C '#{remote_path}' --strip-components=1"
        ]
        sprite_process = Process.new(
          "sprite",
          sprite_cmd_args,
          input: Process::Redirect::Pipe,
          output: Process::Redirect::Inherit,
          error: Process::Redirect::Inherit
        )

        IO.copy(tar_process.output, sprite_process.input)
        sprite_process.input.close
        tar_process.output.close

        tar_process.wait
        sprite_process.wait
      else
        puts "Pushing file: #{local_path} -> #{remote_path}"

        remote_dir = File.dirname(remote_path)
        mkdir_cmd = "mkdir -p '#{remote_dir}' && cat > '#{remote_path}'"
        data = File.read(local_path)

        input = IO::Memory.new(data)
        output = IO::Memory.new

        sprite_cmd_args = ["exec"] + sprite_args + ["bash", "-c", mkdir_cmd]
        status = Process.run(
          "sprite",
          sprite_cmd_args,
          input: input,
          output: output,
          error: Process::Redirect::Inherit
        )

        unless status.success?
          STDERR.puts "Error: push failed (exit #{status.exit_code})"
          exit 1
        end
      end

      puts "Done."
    end
  end
end
