# frozen_string_literal: true

require "open3"
require "shellwords"
require "fileutils"

module SpriteTool
  module Pull
    # Pull a file or directory from a sprite to the local filesystem.
    #
    # remote_path - Path on the sprite to pull.
    # local_path  - Local destination path.
    # sprite_name - Optional sprite name (uses default if nil/empty).
    def self.run(remote_path, local_path, sprite_name)
      sprite_args = []
      sprite_args.push("-s", sprite_name) if sprite_name && !sprite_name.empty?

      # Check if remote path is a directory or file
      remote_escaped = remote_path.shellescape
      is_dir_out, status = Open3.capture2(
        "sprite", "exec", *sprite_args, "bash", "-c",
        "[ -d #{remote_escaped} ] && echo dir || echo file"
      )

      unless status.success?
        $stderr.puts "Error: could not stat remote path #{remote_path}"
        exit 1
      end

      is_dir = is_dir_out.strip == "dir"

      if is_dir
        puts "Pulling directory: #{remote_path} -> #{local_path}"
        FileUtils.mkdir_p(local_path)

        # sprite exec tar on remote, pipe into local tar extract
        sprite_cmd = [
          "sprite", "exec", *sprite_args, "tar", "czf", "-", "-C", remote_path, "."
        ]
        tar_cmd = ["tar", "xzf", "-", "-C", local_path]

        Open3.pipeline(sprite_cmd, tar_cmd)
      else
        puts "Pulling file: #{remote_path} -> #{local_path}"
        FileUtils.mkdir_p(File.dirname(local_path))

        out, status = Open3.capture2(
          "sprite", "exec", *sprite_args, "cat", remote_path
        )

        unless status.success?
          $stderr.puts "Error: could not read remote file #{remote_path}"
          exit 1
        end

        File.binwrite(local_path, out)
      end

      puts "Done."
    end
  end
end
