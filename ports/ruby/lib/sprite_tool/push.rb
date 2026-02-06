# frozen_string_literal: true

require "open3"
require "shellwords"
require "pathname"

module SpriteTool
  module Push
    # Push a local file or directory to a sprite.
    #
    # local_path  - Path to a local file or directory.
    # remote_path - Destination path on the sprite.
    # sprite_name - Optional sprite name (uses default if nil/empty).
    def self.run(local_path, remote_path, sprite_name)
      unless File.exist?(local_path)
        $stderr.puts "Error: #{local_path} does not exist"
        exit 1
      end

      sprite_args = []
      sprite_args.push("-s", sprite_name) if sprite_name && !sprite_name.empty?

      if File.directory?(local_path)
        puts "Pushing directory: #{local_path} -> #{remote_path}"

        parent = File.dirname(local_path)
        base = File.basename(local_path)
        remote_escaped = remote_path.shellescape

        tar_cmd = ["tar", "czf", "-", "-C", parent, base]
        sprite_cmd = [
          "sprite", "exec", *sprite_args, "bash", "-c",
          "mkdir -p #{remote_escaped} && tar xzf - -C #{remote_escaped} --strip-components=1"
        ]

        Open3.pipeline(tar_cmd, sprite_cmd)
      else
        puts "Pushing file: #{local_path} -> #{remote_path}"

        remote_dir = File.dirname(remote_path)
        remote_dir_escaped = remote_dir.shellescape
        remote_escaped = remote_path.shellescape

        mkdir_cmd = "mkdir -p #{remote_dir_escaped} && cat > #{remote_escaped}"
        data = File.binread(local_path)

        Open3.capture2(
          "sprite", "exec", *sprite_args, "bash", "-c", mkdir_cmd,
          stdin_data: data
        )
      end

      puts "Done."
    end
  end
end
