# Смена размера шрифта по Ctrl-Up/Down
#   :PROPERTIES:
#   :header-args+:   :tangle   ~/.urxvt/ext/el-public-set-font-size :tangle-mode (identity #o755)
#   :END:

# Кнопки настроены через Xresources


# [[file:../../org/personal/blogs/it/dotfiles.sorg::*Смена размера шрифта по Ctrl-Up/Down][Смена размера шрифта по Ctrl-Up/Down:1]]
use strict;
use warnings;

my %escapecodes = (
    "font"           => 710,
    "boldFont"       => 711,
    "italicFont"     => 712,
    "boldItalicFont" => 713
);

sub on_start
{
    my ($self) = @_;

    $self->{step} = $self->x_resource("%.step") || 1;

    foreach my $type (qw(font boldFont italicFont boldItalicFont)) {
        $self->{$type} = $self->x_resource($type) || "undef";
    }
}

sub on_user_command
{
    my ($self, $cmd) = @_;

    my $step = $self->{step};

    if ($cmd eq "font-size:increase") {
        fonts_change_size($self,  $step, 0);
    } elsif ($cmd eq "font-size:decrease") {
        fonts_change_size($self, -$step, 0);
    } elsif ($cmd eq "font-size:incglobal") {
        fonts_change_size($self,  $step, 1);
    } elsif ($cmd eq "font-size:decglobal") {
        fonts_change_size($self, -$step, 1);
    } elsif ($cmd eq "font-size:incsave") {
        fonts_change_size($self,  $step, 2);
    } elsif ($cmd eq "font-size:decsave") {
        fonts_change_size($self, -$step, 2);
    } elsif ($cmd eq "font-size:reset") {
        fonts_reset($self);
    }
}

sub fonts_change_size
{
    my ($term, $change, $save) = @_;

    my @newfonts = ();

    my $curres = $term->resource('font');
    if (!$curres) {
        $term->scr_add_lines("\r\nWarning: No font configured, trying a default.\r\nPlease set a font with the 'URxvt.font' resource.");
        $curres = "fixed";
    }
    my @curfonts = split(/,/, $curres);

    my $basefont = shift(@curfonts);
    my ($newbasefont, $newbasesize) = handle_font($term, $basefont, $change, 0);
    push @newfonts, $newbasefont;

    # Only adjust other fonts if base font changed
    if ($newbasefont ne $basefont) {
        foreach my $font (@curfonts) {
            my ($newfont, $newsize) = handle_font($term, $font, $change, $newbasesize);
            push @newfonts, $newfont;
        }
        my $newres = join(",", @newfonts);
        font_apply_new($term, $newres, "font", $save);

        handle_type($term, "boldFont",       $change, $newbasesize, $save);
        handle_type($term, "italicFont",     $change, $newbasesize, $save);
        handle_type($term, "boldItalicFont", $change, $newbasesize, $save);
    }

    if ($save > 1) {
        # write the new values back to the file
        my $xresources = readlink $ENV{"HOME"} . "/.Xresources";
        system("xrdb -edit " . $xresources);
    }
}

sub fonts_reset
{
    my ($term) = @_;

    foreach my $type (qw(font boldFont italicFont boldItalicFont)) {
        my $initial = $term->{$type};
        if ($initial ne "undef") {
            font_apply_new($term, $initial, $type, 0);
        }
    }
}

sub handle_type
{
    my ($term, $type, $change, $basesize, $save) = @_;

    my $curres = $term->resource($type);
    if (!$curres) {
        return;
    }
    my @curfonts = split(/,/, $curres);
    my @newfonts = ();

    foreach my $font (@curfonts) {
        my ($newfont, $newsize) = handle_font($term, $font, $change, $basesize);
        push @newfonts, $newfont;
    }

    my $newres = join(",", @newfonts);
    font_apply_new($term, $newres, $type, $save);
}

sub handle_font
{
    my ($term, $font, $change, $basesize) = @_;

    my $newfont;
    my $newsize;
    my $prefix = 0;

    if ($font =~ /^\s*x:/) {
        $font =~ s/^\s*x://;
        $prefix = 1;
    }
    if ($font =~ /^\s*(\[.*\])?xft:/) {
        ($newfont, $newsize) = font_change_size_xft($term, $font, $change, $basesize);
    } elsif ($font =~ /^\s*-/) {
        ($newfont, $newsize) = font_change_size_xlfd($term, $font, $change, $basesize);
    } else {
        # check whether the font is a valid alias and if yes resolve it to the
        # actual font
        my $lsfinfo = `xlsfonts -l $font 2>/dev/null`;

        if ($lsfinfo eq "") {
            # not a valid alias, ring the bell if it is the base font and just
            # return the current font
            if ($basesize == 0) {
                $term->scr_bell;
            }
            return ($font, $basesize);
        }

        my $fontinfo = (split(/\n/, $lsfinfo))[-1];
        my ($fontfull) = ($fontinfo =~ /\s+([-a-z0-9]+$)/);
        ($newfont, $newsize) = font_change_size_xlfd($term, $fontfull, $change, $basesize);
    }

    # $term->scr_add_lines("\r\nNew font is $newfont\n");
    if ($prefix) {
        $newfont = "x:$newfont";
    }
    return ($newfont, $newsize);
}

sub font_change_size_xft
{
    my ($term, $fontstring, $change, $basesize) = @_;

    my @pieces   = split(/:/, $fontstring);
    my @resized  = ();
    my $size     = 0;
    my $new_size = 0;

    foreach my $piece (@pieces) {
        if ($piece =~ /^(?:(?:pixel)?size=|[^=-]+-)(\d+(\.\d*)?)$/) {
            $size = $1;

            if ($basesize != 0) {
                $new_size = $basesize;
            } else {
                $new_size = $size + $change
            }

            $piece =~ s/(=|-)$size/$1$new_size/;
        }
        push @resized, $piece;
    }

    my $resized_str = join(":", @resized);

    # don't make fonts too small
    if ($new_size >= 6) {
        return ($resized_str, $new_size);
    } else {
        if ($basesize == 0) {
            $term->scr_bell;
        }
        return ($fontstring, $size);
    }
}

sub font_change_size_xlfd
{
    my ($term, $fontstring, $change, $basesize) = @_;

    #-xos4-terminus-medium-r-normal-*-12-*-*-*-*-*-*-1

    my @fields = qw(foundry family weight slant setwidth style pixelSize pointSize Xresolution Yresolution spacing averageWidth registry encoding);

    my %font;
    $fontstring =~ s/^-//;  # Strip leading - before split
    @font{@fields} = split(/-/, $fontstring);

    if ($font{pixelSize} eq '*') {
        $term->scr_add_lines("\r\nWarning: Font size undefined, assuming 12.\r\nPlease set the 'URxvt.font' resource to a font with a concrete size.");
        $font{pixelSize} = '12'
    }
    if ($font{registry} eq '*') {
        $font{registry} ='iso8859';
    }

    # Blank out the size for the pattern
    my %pattern = %font;
    $pattern{foundry} = '*';
    $pattern{setwidth} = '*';
    $pattern{pixelSize} = '*';
    $pattern{pointSize} = '*';
    # if ($basesize != 0) {
    #     $pattern{Xresolution} = '*';
    #     $pattern{Yresolution} = '*';
    # }
    $pattern{averageWidth} = '*';
    # make sure there are no empty fields
    foreach my $field (@fields) {
        $pattern{$field} = '*' unless defined($pattern{$field});
    }
    my $new_fontstring = '-' . join('-', @pattern{@fields});

    my @possible;
    # $term->scr_add_lines("\r\nPattern is $new_fontstring\n");
    open(FOO, "xlsfonts -fn '$new_fontstring' | sort -u |") or die $!;
    while (<FOO>) {
        chomp;
        s/^-//;  # Strip leading '-' before split
        my @fontdata = split(/-/, $_);

        push @possible, [$fontdata[6], "-$_"];
        # $term->scr_add_lines("\r\npossibly $fontdata[6] $_\n");
    }
    close(FOO);

    if (!@possible) {
        die "No possible fonts!";
    }

    if ($basesize != 0) {
        # sort by font size, descending
        @possible = sort {$b->[0] <=> $a->[0]} @possible;

        # font is not the base font, so find the largest font that is at most
        # as large as the base font. If the largest possible font is smaller
        # than the base font bail and hope that a 0-size font can be found at
        # the end of the function
        if ($possible[0]->[0] > $basesize) {
            foreach my $candidate (@possible) {
                if ($candidate->[0] <= $basesize) {
                    return ($candidate->[1], $candidate->[0]);
                }
            }
        }
    } elsif ($change > 0) {
        # sort by font size, ascending
        @possible = sort {$a->[0] <=> $b->[0]} @possible;

        foreach my $candidate (@possible) {
            if ($candidate->[0] >= $font{pixelSize} + $change) {
                return ($candidate->[1], $candidate->[0]);
            }
        }
    } elsif ($change < 0) {
        # sort by font size, descending
        @possible = sort {$b->[0] <=> $a->[0]} @possible;

        foreach my $candidate (@possible) {
            if ($candidate->[0] <= $font{pixelSize} + $change && $candidate->[0] != 0) {
                return ($candidate->[1], $candidate->[0]);
            }
        }
    }

    # no fitting font available, check whether a 0-size font can be used to
    # fit the size of the base font
    @possible = sort {$a->[0] <=> $b->[0]} @possible;
    if ($basesize != 0 && $possible[0]->[0] == 0) {
        return ($possible[0]->[1], $basesize);
    } else {
        # if there is absolutely no smaller/larger font that can be used
        # return the current one, and beep if this is the base font
        if ($basesize == 0) {
            $term->scr_bell;
        }
        return ("-$fontstring", $font{pixelSize});
    }
}

sub font_apply_new
{
    my ($term, $newfont, $type, $save) = @_;

    # $term->scr_add_lines("\r\nnew font is $newfont\n");

    $term->cmd_parse("\033]" . $escapecodes{$type} . ";" . $newfont . "\033\\");

    # load the xrdb db
    # system("xrdb -load " . X_RESOURCES);

    if ($save > 0) {
        # merge the new values
        open(XRDB_MERGE, "| xrdb -merge") || die "can't fork: $!";
        local $SIG{PIPE} = sub { die "xrdb pipe broken" };
        print XRDB_MERGE "URxvt." . $type . ": " . $newfont;
        close(XRDB_MERGE) || die "bad xrdb: $! $?";
    }
}
# Смена размера шрифта по Ctrl-Up/Down:1 ends here
