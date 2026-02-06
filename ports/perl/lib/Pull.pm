package Pull;
# sprite-tool pull: Pull file or directory from a sprite.

use strict;
use warnings;
use File::Basename qw(dirname);
use File::Path qw(make_path);

use SpriteExec;

sub usage {
    print <<'EOF';
Usage: sprite-tool pull <remote-path> <local-path> [sprite-name]

Examples:
  sprite-tool pull /home/sprite/file.txt ./file.txt
  sprite-tool pull /home/sprite/mydir ./mydir
  sprite-tool pull /tmp/file.txt ./file.txt ember-red-hawk
EOF
    exit 1;
}

sub run {
    my (@args) = @_;

    usage() unless @args >= 2;

    my $remote_path = $args[0];
    my $local_path  = $args[1];
    my $sprite      = $args[2] // '';

    my $sprite_args = SpriteExec::_sprite_args($sprite);

    # Check if remote path is a directory or file
    my $check_cmd = "sprite exec $sprite_args bash -c "
                  . SpriteExec::_shell_escape("[ -d '$remote_path' ] && echo dir || echo file");
    my $is_dir = `$check_cmd 2>/dev/null`;
    chomp $is_dir if defined $is_dir;
    $is_dir //= 'file';

    if ($is_dir eq 'dir') {
        print "Pulling directory: $remote_path -> $local_path\n";
        make_path($local_path);

        my $pull_cmd = "sprite exec $sprite_args tar czf - -C "
                     . SpriteExec::_shell_escape($remote_path)
                     . " . | tar xzf - -C "
                     . SpriteExec::_shell_escape($local_path);
        system($pull_cmd);
    } else {
        print "Pulling file: $remote_path -> $local_path\n";
        my $local_dir = dirname($local_path);
        make_path($local_dir) if $local_dir ne '.' && $local_dir ne '';

        my $cat_cmd = "sprite exec $sprite_args cat "
                    . SpriteExec::_shell_escape($remote_path);
        my $content = `$cat_cmd`;

        open my $fh, '>', $local_path or do {
            print STDERR "Error: cannot write to $local_path: $!\n";
            exit 1;
        };
        binmode $fh;
        print $fh $content if defined $content;
        close $fh;
    }

    print "Done.\n";
    return 0;
}

1;
