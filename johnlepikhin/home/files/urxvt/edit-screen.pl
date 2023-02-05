use warnings;
use strict;
use File::Temp;

sub on_init {
   my ($self) = @_;

   # only for backwards compatibility
   my $hotkey = $self->{argv}[0]
                || $self->x_resource (q{%})
                || "M-e";

   $self->bind_action ($hotkey, "%:edit_screen_action")
      or warn "unable to register '$hotkey' as scrollback search start hotkey\n";

   return
}

sub edit_screen {
    my $self = shift;

    my $history = q{};
    my $row     = $self->top_row;
    while ( $self->nrow > $row ) {
        my $line = $self->line($row);
        $history .= $line->t() . "\n";
        $row++;
    }

    my $fh = File::Temp->new( TEMPLATE => '/tmp/urxvt_edit_screen_XXXXXX' );
    binmode $fh, ":encoding(utf-8)";
    print {$fh} $history;
    close $fh;

    system 'emacsclient', '--create-frame', $fh->filename;

    return
}

sub on_action {
    my $self   = shift;
    my $action = shift;

    if ( $action eq 'edit_screen_action' ) {
        edit_screen($self);
    }

    return
}

1;
