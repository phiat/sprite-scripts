module SpriteTool
  module Pull
    # Pull a file or directory from a sprite to the local filesystem.
    #
    # remote_path - Path on the sprite to pull.
    # local_path  - Local destination path.
    # sprite_name - Optional sprite name (uses default if nil/empty).
    def self.run(remote_path : String, local_path : String, sprite_name : String?)
      sprite_args = [] of String
      if sprite_name && !sprite_name.empty?
        sprite_args.push("-s", sprite_name)
      end

      # Check if remote path is a directory or file
      check_cmd_args = ["exec"] + sprite_args + ["bash", "-c",
        "[ -d '#{remote_path}' ] && echo dir || echo file"
      ]
      output = IO::Memory.new
      status = Process.run("sprite", check_cmd_args, output: output, error: Process::Redirect::Close)

      unless status.success?
        STDERR.puts "Error: could not stat remote path #{remote_path}"
        exit 1
      end

      is_dir = output.to_s.strip == "dir"

      if is_dir
        puts "Pulling directory: #{remote_path} -> #{local_path}"
        Dir.mkdir_p(local_path)

        # sprite exec tar on remote, pipe into local tar extract
        sprite_cmd_args = ["exec"] + sprite_args + ["tar", "czf", "-", "-C", remote_path, "."]
        sprite_process = Process.new(
          "sprite",
          sprite_cmd_args,
          output: Process::Redirect::Pipe,
          error: Process::Redirect::Inherit
        )

        tar_process = Process.new(
          "tar",
          ["xzf", "-", "-C", local_path],
          input: Process::Redirect::Pipe,
          output: Process::Redirect::Inherit,
          error: Process::Redirect::Inherit
        )

        IO.copy(sprite_process.output, tar_process.input)
        tar_process.input.close
        sprite_process.output.close

        sprite_process.wait
        tar_process.wait
      else
        puts "Pulling file: #{remote_path} -> #{local_path}"
        local_dir = File.dirname(local_path)
        Dir.mkdir_p(local_dir)

        file_output = IO::Memory.new
        cat_cmd_args = ["exec"] + sprite_args + ["cat", remote_path]
        cat_status = Process.run("sprite", cat_cmd_args, output: file_output, error: Process::Redirect::Close)

        unless cat_status.success?
          STDERR.puts "Error: could not read remote file #{remote_path}"
          exit 1
        end

        File.write(local_path, file_output.to_s)
      end

      puts "Done."
    end
  end
end
