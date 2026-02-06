package Push;
# sprite-tool push: Push local file or directory to a sprite.

use strict;
use warnings;
use File::Basename qw(dirname);

use SpriteExec;

sub usage {
    print <<'EOF';
Usage: sprite-tool push <local-path> <remote-path> [sprite-name]

Examples:
  sprite-tool push ./file.txt /home/sprite/file.txt
  sprite-tool push ./mydir /home/sprite/mydir
  sprite-tool push ./file.txt /tmp/file.txt ember-red-hawk
EOF
    exit 1;
}

sub run {
    my (@args) = @_;

    usage() unless @args >= 2;

    my $local_path  = $args[0];
    my $remote_path = $args[1];
    my $sprite      = $args[2] // '';

    if (! -e $local_path) {
        print STDERR "Error: $local_path does not exist\n";
        exit 1;
    }

    if (-d $local_path) {
        print "Pushing directory: $local_path -> $remote_path\n";
        SpriteExec::push_dir_strip($sprite, $local_path, $remote_path, 0);
    } else {
        print "Pushing file: $local_path -> $remote_path\n";
        SpriteExec::push_file($sprite, $local_path, $remote_path, 0);
    }

    print "Done.\n";
    return 0;
}

1;
