S.bind('return:alt', function (w) {
  S.shell("/usr/bin/open -n -a iTerm");
});

S.bind('i:alt', function (w) {
  w.doOperation(S.operation('focus', { app: 'iTerm2' }));
});

S.bind('c:alt', function (w) {
  w.doOperation(S.operation('focus', { app: 'Google Chrome' }));
});

S.bind('a:alt', function (w) {
  w.doOperation(S.operation('focus', { app: 'Atom' }));
});

S.bind('k:alt', function (w) {
  w.doOperation(S.operation('focus', { app: 'Keynote' }));
});
