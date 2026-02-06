# frozen_string_literal: true

require "open3"
require "shellwords"

module SpriteTool
  module Watch
    # Watch a sprite's beads tracker task, polling for progress.
    #
    # sprite_name   - Name of the sprite to watch.
    # task_id       - Beads task ID (auto-detected if nil/empty).
    # poll_interval - Seconds between polls (default 30).
    def self.run(sprite_name, task_id, poll_interval)
      poll_interval = poll_interval.to_i
      poll_interval = 30 if poll_interval <= 0

      sprite = Sprite.new(sprite_name)

      # Auto-detect tracker task if not specified
      if task_id.nil? || task_id.empty?
        puts "Detecting tracker task..."

        task_id = detect_task(sprite, "--priority critical")

        if task_id.nil? || task_id.empty?
          puts "No critical task found. Falling back to first open task..."
          task_id = detect_task(sprite, "")
        end

        if task_id.nil? || task_id.empty?
          $stderr.puts "ERROR: No beads tasks found on sprite '#{sprite_name}'"
          $stderr.puts "Specify a task ID manually: sprite-tool watch #{sprite_name} <task-id>"
          exit 1
        end

        puts "Tracking task: #{task_id}"
      end

      puts "Watching sprite '#{sprite_name}' task '#{task_id}' (every #{poll_interval}s)"
      puts "Press Ctrl+C to stop"
      puts ""

      loop do
        # Clear screen: ANSI escape sequences
        print "\e[2J\e[H"

        time_str = Time.now.strftime("%H:%M:%S")
        puts "=== sprite-watch: #{sprite_name} / #{task_id} === #{time_str} ==="
        puts ""

        # Show task status
        begin
          out = sprite.exec("cd /home/sprite && bd show #{task_id.shellescape} 2>/dev/null")
          puts out
        rescue
          puts "(could not read task)"
        end
        puts ""

        # Show recent comments
        puts "--- Recent updates ---"
        begin
          comments = sprite.exec(
            "cd /home/sprite && bd comments #{task_id.shellescape} 2>/dev/null | tail -8"
          )
          puts comments
        rescue
          puts "(no comments)"
        end
        puts ""

        # Check if done
        begin
          status_line = sprite.exec(
            "cd /home/sprite && bd show #{task_id.shellescape} 2>/dev/null | grep -i status"
          )
          if status_line =~ /closed|done|completed/i
            puts "=========================================="
            puts "PROJECT COMPLETE"
            puts "=========================================="
            break
          end
        rescue
          # status check failed, continue polling
        end

        sleep poll_interval
      end
    end

    # Detect the first task from `bd list` with optional extra args.
    def self.detect_task(sprite, extra_args)
      cmd = "cd /home/sprite && bd list #{extra_args} 2>/dev/null | head -1 | awk '{print $1}'"
      result = sprite.exec(cmd)
      result.empty? ? nil : result
    rescue
      nil
    end

    private_class_method :detect_task
  end
end
