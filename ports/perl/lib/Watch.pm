package Watch;
# sprite-tool watch: Poll a sprite's beads tracker task for progress.

use strict;
use warnings;

use SpriteExec;

sub usage {
    print <<'EOF';
Usage: sprite-tool watch <sprite-name> [task-id] [poll-interval]

Arguments:
  sprite-name     Name of the sprite to watch
  task-id         Beads task ID to track (default: auto-detect first open critical task)
  poll-interval   Seconds between polls (default: 30)

Examples:
  sprite-tool watch ember-red-hawk
  sprite-tool watch ember-red-hawk CRM-1
  sprite-tool watch ember-red-hawk CRM-1 60
EOF
    exit 1;
}

sub run {
    my (@args) = @_;

    usage() unless @args >= 1;

    my $sprite        = $args[0];
    my $task_id       = $args[1] // '';
    my $poll_interval = $args[2] // 30;

    # Auto-detect tracker task if not specified
    if ($task_id eq '') {
        print "Detecting tracker task...\n";
        $task_id = SpriteExec::sx_capture($sprite,
            q{cd /home/sprite && bd list --priority critical 2>/dev/null | head -1 | awk '{print $1}'});

        if (!defined $task_id || $task_id eq '') {
            print "No critical task found. Falling back to first open task...\n";
            $task_id = SpriteExec::sx_capture($sprite,
                q{cd /home/sprite && bd list 2>/dev/null | head -1 | awk '{print $1}'});
        }

        if (!defined $task_id || $task_id eq '') {
            print STDERR "ERROR: No beads tasks found on sprite '$sprite'\n";
            print STDERR "Specify a task ID manually: sprite-tool watch $sprite <task-id>\n";
            exit 1;
        }

        print "Tracking task: $task_id\n";
    }

    print "Watching sprite '$sprite' task '$task_id' (every ${poll_interval}s)\n";
    print "Press Ctrl+C to stop\n\n";

    while (1) {
        # Clear screen (ANSI escape)
        print "\e[2J\e[H";

        my @t = localtime;
        my $ts = sprintf "%02d:%02d:%02d", $t[2], $t[1], $t[0];
        print "=== sprite-watch: $sprite / $task_id === $ts ===\n\n";

        # Show task status
        my $task_output = SpriteExec::sx_capture($sprite,
            "cd /home/sprite && bd show $task_id 2>/dev/null");
        if (defined $task_output && $task_output ne '') {
            print "$task_output\n";
        } else {
            print "(could not read task)\n";
        }
        print "\n";

        # Show recent comments
        print "--- Recent updates ---\n";
        my $comments = SpriteExec::sx_capture($sprite,
            "cd /home/sprite && bd comments $task_id 2>/dev/null | tail -8");
        if (defined $comments && $comments ne '') {
            print "$comments\n";
        } else {
            print "(no comments)\n";
        }
        print "\n";

        # Check if done
        my $status = SpriteExec::sx_capture($sprite,
            "cd /home/sprite && bd show $task_id 2>/dev/null | grep -i status");
        if (defined $status && $status =~ /closed|done|completed/i) {
            print "==========================================\n";
            print "PROJECT COMPLETE\n";
            print "==========================================\n";
            last;
        }

        sleep $poll_interval;
    }

    return 0;
}

1;
