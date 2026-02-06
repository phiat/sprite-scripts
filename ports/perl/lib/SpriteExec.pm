package SpriteExec;
# Sprite CLI execution helpers: sx, push_file, push_dir.

use strict;
use warnings;
use File::Basename qw(dirname basename);
use IPC::Open3;
use POSIX qw(:sys_wait_h);

# Shell-escape a string for safe embedding in single quotes.
# Replace each ' with '\'' (end quote, escaped quote, start quote).
sub _shell_escape {
    my ($str) = @_;
    $str =~ s/'/'\\''/g;
    return "'$str'";
}

# Run a command inside the sprite via bash.
# sx($sprite, $cmd, $dry_run)
sub sx {
    my ($sprite, $cmd, $dry_run) = @_;
    $dry_run //= 0;

    if ($dry_run) {
        print "  [dry-run] sprite exec -s $sprite bash -c \"$cmd\"\n";
        return 0;
    }

    my $escaped = _shell_escape($cmd);
    my $full_cmd = "sprite exec -s " . _shell_escape($sprite) . " bash -c $escaped";
    my $rc = system($full_cmd);
    return $rc >> 8;
}

# Run a command inside the sprite and capture stdout.
# Returns the captured output string.
sub sx_capture {
    my ($sprite, $cmd, $dry_run) = @_;
    $dry_run //= 0;

    if ($dry_run) {
        print "  [dry-run] sprite exec -s $sprite bash -c \"$cmd\"\n";
        return '';
    }

    my $escaped = _shell_escape($cmd);
    my $full_cmd = "sprite exec -s " . _shell_escape($sprite) . " bash -c $escaped";
    my $output = `$full_cmd 2>/dev/null`;
    chomp $output if defined $output;
    return $output // '';
}

# Build sprite args array based on whether sprite name is provided.
# Returns the args string fragment (e.g., "-s my-sprite" or "").
sub _sprite_args {
    my ($sprite) = @_;
    if (defined $sprite && $sprite ne '') {
        return "-s " . _shell_escape($sprite);
    }
    return '';
}

# Push a local file to the sprite.
# push_file($sprite, $src, $dest, $dry_run)
sub push_file {
    my ($sprite, $src, $dest, $dry_run) = @_;
    $dry_run //= 0;

    if ($dry_run) {
        print "  [dry-run] push $src -> sprite:$dest\n";
        return 0;
    }

    my $dest_dir = dirname($dest);
    my $sprite_args = _sprite_args($sprite);

    # Create remote directory
    my $mkdir_cmd = "sprite exec $sprite_args bash -c " . _shell_escape("mkdir -p '$dest_dir'");
    system($mkdir_cmd);

    # Pipe file contents to sprite
    my $cat_cmd = "sprite exec $sprite_args bash -c " . _shell_escape("cat > '$dest'");

    open my $fh, '<', $src or do {
        warn "Error: cannot open $src: $!\n";
        return 1;
    };

    # Use open with pipe to feed stdin
    open my $pipe, '|-', $cat_cmd or do {
        warn "Error: cannot run sprite exec: $!\n";
        close $fh;
        return 1;
    };

    binmode $fh;
    binmode $pipe;

    while (my $bytes_read = read($fh, my $buf, 8192)) {
        print $pipe $buf;
    }

    close $fh;
    close $pipe;
    return $? >> 8;
}

# Push a local directory to the sprite.
# push_dir($sprite, $src, $dest, $dry_run)
sub push_dir {
    my ($sprite, $src, $dest, $dry_run) = @_;
    $dry_run //= 0;

    if ($dry_run) {
        print "  [dry-run] push dir $src -> sprite:$dest\n";
        return 0;
    }

    my $sprite_args = _sprite_args($sprite);

    # Create remote directory
    my $mkdir_cmd = "sprite exec $sprite_args bash -c " . _shell_escape("mkdir -p '$dest'");
    system($mkdir_cmd);

    # tar pipe: local tar -> sprite tar extract
    my $src_dir  = dirname($src);
    my $src_base = basename($src);
    my $dest_parent = dirname($dest);

    my $tar_cmd = "tar czf - -C " . _shell_escape($src_dir) . " " . _shell_escape($src_base)
                . " | sprite exec $sprite_args bash -c "
                . _shell_escape("tar xzf - -C '$dest_parent'");

    my $rc = system($tar_cmd);
    return $rc >> 8;
}

# Push a local directory to the sprite with --strip-components=1
# (used by standalone push subcommand where remote path IS the target dir).
# push_dir_strip($sprite, $src, $dest, $dry_run)
sub push_dir_strip {
    my ($sprite, $src, $dest, $dry_run) = @_;
    $dry_run //= 0;

    if ($dry_run) {
        print "  [dry-run] push dir $src -> sprite:$dest\n";
        return 0;
    }

    my $sprite_args = _sprite_args($sprite);

    my $src_dir  = dirname($src);
    my $src_base = basename($src);

    my $tar_cmd = "tar czf - -C " . _shell_escape($src_dir) . " " . _shell_escape($src_base)
                . " | sprite exec $sprite_args bash -c "
                . _shell_escape("mkdir -p '$dest' && tar xzf - -C '$dest' --strip-components=1");

    my $rc = system($tar_cmd);
    return $rc >> 8;
}

1;
