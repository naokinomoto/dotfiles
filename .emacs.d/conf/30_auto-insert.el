(require 'autoinsert)

(defconst perl-auto-insert '(("\\.pl\\'". "Perl Script")))
(setcdr
 perl-auto-insert
 '("
#!/usr/bin/perl

use strict;
use warnings;
"
))
