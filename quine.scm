(define X '(#\) #\) #\  #\( #\d #\i #\s #\p #\l #\a #\y #\  #\" #\( #\d #\e #\f #\i #\n #\e #\  #\X #\  #\' #\( #\" #\) #\  #\( #\l #\e #\t #\  #\f #\  #\( #\( #\x #\  #\X #\) #\) #\  #\( #\c #\o #\n #\d #\  #\( #\( #\n #\u #\l #\l #\? #\  #\x #\) #\  #\1 #\) #\  #\( #\e #\l #\s #\e #\  #\( #\d #\i #\s #\p #\l #\a #\y #\  #\" #\# #\\ #\\ #\" #\) #\  #\( #\d #\i #\s #\p #\l #\a #\y #\  #\( #\c #\a #\r #\  #\x #\) #\) #\  #\( #\d #\i #\s #\p #\l #\a #\y #\  #\" #\  #\" #\) #\  #\( #\f #\  #\( #\c #\d #\r #\  #\x #\) #\) #\) #\) #\) #\  #\( #\l #\e #\t #\  #\g #\  #\( #\( #\x #\  #\X #\) #\) #\  #\( #\c #\o #\n #\d #\  #\( #\( #\n #\u #\l #\l #\? #\  #\x #\) #\  #\1 #\) #\  #\( #\e #\l #\s #\e #\  #\( #\d #\i #\s #\p #\l #\a #\y #\  #\( #\c #\a #\r #\  #\x #\) #\) #\  #\( #\g #\  #\( #\c #\d #\r #\  #\x #\) #\) #\) #\) #\) #\  #\; #\  #\b #\r #\o #\  #\y #\e #\s )) (display "(define X '(") (let f ((x X)) (cond ((null? x) 1) (else (display "#\\") (display (car x)) (display " ") (f (cdr x))))) (let g ((x X)) (cond ((null? x) 1) (else (display (car x)) (g (cdr x))))) ; bro yes
