package SpriteConfig;
# Configuration: .env parsing and environment variable loading.
# Named SpriteConfig to avoid collision with Perl's core Config.pm module.

use strict;
use warnings;
use File::Basename qw(dirname);

# Parse a .env file, set values into %ENV (don't overwrite existing).
# Returns a hash ref of parsed key-value pairs.
sub parse_env_file {
    my ($path) = @_;
    my %parsed;

    return \%parsed unless defined $path && -f $path;

    open my $fh, '<', $path or do {
        warn "Warning: could not open $path: $!\n";
        return \%parsed;
    };

    while (my $line = <$fh>) {
        chomp $line;
        $line =~ s/^\s+//;
        $line =~ s/\s+$//;

        # Skip blank lines and comments
        next if $line eq '' || $line =~ /^#/;

        # Must contain =
        next unless $line =~ /=/;

        my ($key, $value) = split /=/, $line, 2;
        $key   =~ s/^\s+//;
        $key   =~ s/\s+$//;
        $value //= '';
        $value =~ s/^\s+//;
        $value =~ s/\s+$//;

        # Strip matching quotes
        if (length($value) >= 2) {
            my $first = substr($value, 0, 1);
            my $last  = substr($value, -1, 1);
            if ($first eq $last && ($first eq '"' || $first eq "'")) {
                $value = substr($value, 1, length($value) - 2);
            }
        }

        $parsed{$key} = $value;

        # Set in %ENV (don't overwrite existing)
        if (!exists $ENV{$key}) {
            $ENV{$key} = $value;
        }
    }

    close $fh;
    return \%parsed;
}

# Load configuration from .env file and environment variables.
# Returns a hash ref with all config fields.
sub load {
    my $env_file = $ENV{ENV_FILE} // './.env';

    # Parse .env file first (populates %ENV for keys not already set)
    parse_env_file($env_file);

    # SPRITE_TOKEN with SPRITES_TOKEN fallback
    my $sprite_token = $ENV{SPRITE_TOKEN} // '';
    if ($sprite_token eq '') {
        $sprite_token = $ENV{SPRITES_TOKEN} // '';
    }

    # Agent
    my $agent = $ENV{AGENT} // 'opencode';

    # Claude auth
    my $claude_auth = $ENV{CLAUDE_AUTH} // 'subscription';

    # Anthropic API key
    my $anthropic_api_key = $ENV{ANTHROPIC_API_KEY} // '';

    # Model
    my $model = $ENV{MODEL} // '';

    # Checkpoint interval
    my $interval_str = $ENV{CHECKPOINT_INTERVAL} // '300';
    my $checkpoint_interval;
    if ($interval_str =~ /^\d+$/) {
        $checkpoint_interval = int($interval_str);
    } else {
        die "Error: invalid CHECKPOINT_INTERVAL '$interval_str' (must be integer)\n";
    }

    return {
        sprite_token        => $sprite_token,
        agent               => $agent,
        claude_auth         => $claude_auth,
        anthropic_api_key   => $anthropic_api_key,
        model               => $model,
        checkpoint_interval => $checkpoint_interval,
        env_file            => $env_file,

        # CLI flags (set by caller)
        dry_run       => 0,
        checkpointing => 1,
        upload_dirs   => [],
    };
}

1;
