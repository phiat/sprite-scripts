package Launch;
# sprite-tool launch: Create and configure a sprite with coding agent, git, beads.

use strict;
use warnings;
use File::Basename qw(dirname basename);
use POSIX qw(:sys_wait_h);

use SpriteConfig;
use SpriteExec;

my $checkpoint_pid;

sub usage {
    print <<'EOF';
Usage: sprite-tool launch [options] <sprite-name> [plan-file]

Options:
  --dry-run              Show what would happen without executing
  --no-checkpoint        Disable auto-checkpointing
  --upload <dir>         Upload a local directory to /home/sprite/<dirname>
                         (repeatable: --upload ./data --upload ./tests)

Environment variables:
  ENV_FILE               Path to .env file (default: ./.env)
  SPRITE_TOKEN           sprites.dev API token (or SPRITES_TOKEN in .env)
  AGENT                  "opencode" (default) or "claude"
  CLAUDE_AUTH            "subscription" (default) or "apikey"
  MODEL                  Model override (see below)
  CHECKPOINT_INTERVAL    Seconds between checkpoints (default: 300 = 5min)

Model examples:
  OpenCode: MODEL=opencode/big-pickle  (free, default)
            MODEL=groq/llama-3.3-70b-versatile  (free w/ GROQ_API_KEY)
            MODEL=openai/gpt-4o
            MODEL=google/gemini-2.5-pro
  Claude:   MODEL=opus, MODEL=sonnet, MODEL=haiku

Examples:
  sprite-tool launch my-project plan.md
  sprite-tool launch --upload ./data my-project plan.md
  sprite-tool launch --upload ./data --upload ./tests my-project plan.md
  sprite-tool launch --dry-run my-project plan.md
  MODEL=groq/llama-3.3-70b-versatile sprite-tool launch dev plan.md
  AGENT=claude MODEL=sonnet sprite-tool launch dev plan.md
EOF
    exit 1;
}

sub _start_checkpointing {
    my ($sprite, $interval) = @_;

    my $pid = fork();
    if (!defined $pid) {
        warn "Warning: could not fork for checkpointing: $!\n";
        return;
    }

    if ($pid == 0) {
        # Child process: checkpoint loop
        $SIG{TERM} = sub { exit 0 };
        $SIG{INT}  = sub { exit 0 };
        while (1) {
            sleep $interval;
            my @t = localtime;
            my $ts = sprintf "%02d:%02d:%02d", $t[2], $t[1], $t[0];
            print "[checkpoint] Creating checkpoint at $ts...\n";
            my $rc = system("sprite checkpoint create -s " . SpriteExec::_shell_escape($sprite) . " 2>/dev/null");
            if ($rc == 0) {
                print "[checkpoint] Done.\n";
            } else {
                print "[checkpoint] Failed (non-fatal).\n";
            }
        }
        exit 0;
    }

    # Parent
    $checkpoint_pid = $pid;
    $SIG{CHLD} = 'IGNORE';
    print "Auto-checkpointing every ${interval}s (pid: $checkpoint_pid)\n";
}

sub _stop_checkpointing {
    if (defined $checkpoint_pid && $checkpoint_pid > 0) {
        kill 'TERM', $checkpoint_pid;
        waitpid $checkpoint_pid, 0;
        $checkpoint_pid = undef;
    }
}

sub run {
    my (@args) = @_;

    # Load config (parses .env, reads env vars)
    my $cfg = SpriteConfig::load();

    # Parse flags
    my $dry_run       = 0;
    my $checkpointing = 1;
    my @upload_dirs;

    while (@args && $args[0] =~ /^--/) {
        my $flag = shift @args;
        if ($flag eq '--dry-run') {
            $dry_run = 1;
        } elsif ($flag eq '--no-checkpoint') {
            $checkpointing = 0;
        } elsif ($flag eq '--upload') {
            usage() unless @args;
            push @upload_dirs, shift @args;
        } elsif ($flag eq '--help' || $flag eq '-h') {
            usage();
        } else {
            print STDERR "Unknown option: $flag\n";
            usage();
        }
    }

    usage() unless @args >= 1;

    my $sprite    = shift @args;
    my $plan_file = shift @args;

    # Store into config
    $cfg->{dry_run}       = $dry_run;
    $cfg->{checkpointing} = $checkpointing;
    $cfg->{upload_dirs}   = \@upload_dirs;

    # ============================================================
    # 1. Check/install sprite CLI
    # ============================================================
    my $has_sprite = system("command -v sprite >/dev/null 2>&1") == 0;
    if (!$has_sprite) {
        if ($dry_run) {
            print "  [dry-run] Would install sprite CLI\n";
        } else {
            print "Installing sprite CLI...\n";
            system("curl -fsSL https://sprites.dev/install.sh | sh");
            $ENV{PATH} = "$ENV{HOME}/.local/bin:$ENV{PATH}";
        }
    }

    # ============================================================
    # 2. Auth sprite (non-interactive if token provided)
    # ============================================================
    if ($cfg->{sprite_token} ne '') {
        print "Authenticating sprite with token...\n";
        if (!$dry_run) {
            system("sprite auth setup --token " . SpriteExec::_shell_escape($cfg->{sprite_token}));
        }
    } else {
        print "No SPRITE_TOKEN set. Running interactive login...\n";
        if (!$dry_run) {
            system("sprite login");
        }
    }

    # ============================================================
    # 3. Create sprite (or use existing)
    # ============================================================
    if ($dry_run) {
        print "  [dry-run] Would create or reuse sprite '$sprite'\n";
    } else {
        my $ls_output = `sprite ls 2>/dev/null`;
        if (defined $ls_output && $ls_output =~ /\b\Q$sprite\E\b/) {
            print "Sprite '$sprite' already exists, using it.\n";
        } else {
            print "Creating sprite: $sprite\n";
            system("sprite create -skip-console " . SpriteExec::_shell_escape($sprite));
        }
    }

    # ============================================================
    # 4. Push .env to sprite
    # ============================================================
    if (-f $cfg->{env_file}) {
        print "Pushing $cfg->{env_file}...\n";
        SpriteExec::push_file($sprite, $cfg->{env_file}, '/home/sprite/.env', $dry_run);
    }

    # ============================================================
    # 5. Push plan file if provided
    # ============================================================
    if (defined $plan_file && $plan_file ne '' && -f $plan_file) {
        print "Pushing $plan_file...\n";
        SpriteExec::push_file($sprite, $plan_file, '/home/sprite/plan.md', $dry_run);
    }

    # ============================================================
    # 6. Upload directories if provided
    # ============================================================
    for my $dir (@upload_dirs) {
        if (-d $dir) {
            my $dirname = basename($dir);
            print "Uploading directory: $dir -> /home/sprite/$dirname\n";
            SpriteExec::push_dir($sprite, $dir, "/home/sprite/$dirname", $dry_run);
        } else {
            print "WARNING: --upload dir '$dir' not found, skipping.\n";
        }
    }

    # ============================================================
    # 7. Setup git + beads
    # ============================================================
    print "Initializing git...\n";
    SpriteExec::sx($sprite, 'cd /home/sprite && git init -b main 2>/dev/null || true', $dry_run);

    print "Installing beads...\n";
    SpriteExec::sx($sprite, 'curl -fsSL https://raw.githubusercontent.com/steveyegge/beads/main/scripts/install.sh | bash', $dry_run);

    # ============================================================
    # 8. Install and auth coding agent
    # ============================================================
    my $agent = $cfg->{agent};
    my $model = $cfg->{model};

    if ($agent eq 'claude') {
        print "Setting up claude...\n";
        SpriteExec::sx($sprite, 'command -v claude >/dev/null 2>&1 || npm install -g @anthropic-ai/claude-code', $dry_run);

        if ($cfg->{claude_auth} eq 'subscription') {
            my $creds = "$ENV{HOME}/.claude/.credentials.json";
            if (-f $creds) {
                print "Copying claude subscription credentials...\n";
                SpriteExec::push_file($sprite, $creds, '/home/sprite/.claude/.credentials.json', $dry_run);
                SpriteExec::sx($sprite, 'chmod 600 ~/.claude/.credentials.json', $dry_run);
            } else {
                print STDERR "ERROR: ~/.claude/.credentials.json not found\n";
                print STDERR "Run 'claude' locally first to authenticate, then re-run this script.\n";
                exit 1;
            }
        } elsif ($cfg->{claude_auth} eq 'apikey' && $cfg->{anthropic_api_key} ne '') {
            print "Setting ANTHROPIC_API_KEY in sprite...\n";
            my $api_key = $cfg->{anthropic_api_key};
            SpriteExec::sx($sprite,
                "grep -q ANTHROPIC_API_KEY ~/.bashrc 2>/dev/null || echo 'export ANTHROPIC_API_KEY=\"$api_key\"' >> ~/.bashrc",
                $dry_run);
        } else {
            print STDERR "ERROR: No valid claude auth configured\n";
            print STDERR "Set CLAUDE_AUTH=subscription (default) or CLAUDE_AUTH=apikey with ANTHROPIC_API_KEY\n";
            exit 1;
        }

    } elsif ($agent eq 'opencode') {
        print "Setting up opencode...\n";
        SpriteExec::sx($sprite, '[ -x ~/.opencode/bin/opencode ] || curl -fsSL https://opencode.ai/install | bash', $dry_run);
        SpriteExec::sx($sprite,
            q{grep -q 'source.*\.env' ~/.bashrc 2>/dev/null || echo '[ -f /home/sprite/.env ] && set -a && source /home/sprite/.env && set +a' >> ~/.bashrc},
            $dry_run);
    } else {
        print STDERR "ERROR: Unknown AGENT '$agent'. Use 'claude' or 'opencode'.\n";
        exit 1;
    }

    # ============================================================
    # 9. Launch agent with plan (or open console)
    # ============================================================
    print "\n";
    print "==========================================\n";
    print "Sprite '$sprite' is ready!\n";
    my $model_info = $model ne '' ? " (model: $model)" : '';
    print "Agent: $agent$model_info\n";
    if ($checkpointing) {
        print "Checkpointing: every $cfg->{checkpoint_interval}s\n";
    }
    print "==========================================\n";

    if ($dry_run) {
        print "\n[dry-run] Would launch $agent with plan. No changes were made.\n";
        return 0;
    }

    if (defined $plan_file && $plan_file ne '') {
        # Register cleanup for checkpointing
        # Install END block to stop checkpointing on exit
        my $sprite_copy = $sprite;

        # Start auto-checkpointing before agent runs
        if ($checkpointing) {
            _start_checkpointing($sprite, $cfg->{checkpoint_interval});
        }

        print "Launching $agent with plan...\n";

        if ($agent eq 'claude') {
            my $model_flag = '';
            $model_flag = "--model $model" if $model ne '';
            SpriteExec::sx($sprite,
                "cd /home/sprite && claude $model_flag -p 'read plan.md and complete the plan please'");
        } elsif ($agent eq 'opencode') {
            my $oc_model = $model ne '' ? $model : 'opencode/big-pickle';
            SpriteExec::sx($sprite,
                "set -a && source /home/sprite/.env 2>/dev/null && set +a && cd /home/sprite && ~/.opencode/bin/opencode run -m $oc_model 'read plan.md and complete the plan please'");
        }

        # Final checkpoint after agent completes
        _stop_checkpointing();
        print "Creating final checkpoint...\n";
        my $rc = system("sprite checkpoint create -s " . SpriteExec::_shell_escape($sprite) . " 2>/dev/null");
        if ($rc == 0) {
            print "Final checkpoint saved.\n";
        } else {
            print "Final checkpoint failed (non-fatal).\n";
        }
    } else {
        print "Opening console...\n";
        system("sprite console -s " . SpriteExec::_shell_escape($sprite));
    }

    return 0;
}

# Ensure checkpointing is stopped if the process exits unexpectedly
END {
    _stop_checkpointing();
}

1;
