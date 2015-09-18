var texts = new Array();
var states = new Array();

texts['fold000001'] = '<a href="javascript:fold(\'fold000001\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1 to line 30</i>';
states['fold000001'] = false;
texts['fold000032'] = '<a href="javascript:fold(\'fold000032\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 32 to line 58</i>';
states['fold000032'] = false;
texts['fold000060'] = '<a href="javascript:fold(\'fold000060\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 60 to line 74</i>';
states['fold000060'] = false;
texts['fold000076'] = '<a href="javascript:fold(\'fold000076\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 76 to line 157</i>';
states['fold000076'] = false;
texts['fold000159'] = '<a href="javascript:fold(\'fold000159\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 159 to line 160</i>';
states['fold000159'] = false;
texts['fold000162'] = '<a href="javascript:fold(\'fold000162\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 162 to line 230</i>';
states['fold000162'] = false;
texts['fold000232'] = '<a href="javascript:fold(\'fold000232\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 232 to line 279</i>';
states['fold000232'] = false;
texts['fold000281'] = '<a href="javascript:fold(\'fold000281\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 281 to line 357</i>';
states['fold000281'] = false;
texts['fold000359'] = '<a href="javascript:fold(\'fold000359\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 359 to line 371</i>';
states['fold000359'] = false;
texts['fold000373'] = '<a href="javascript:fold(\'fold000373\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 373 to line 377</i>';
states['fold000373'] = false;
texts['fold000379'] = '<a href="javascript:fold(\'fold000379\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 379 to line 538</i>';
states['fold000379'] = false;
texts['fold000540'] = '<a href="javascript:fold(\'fold000540\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 540 to line 549</i>';
states['fold000540'] = false;

function fold(id) {
  tmp = document.getElementById(id).innerHTML;
  document.getElementById(id).innerHTML = texts[id];
  texts[id] = tmp;
  states[id] = !(states[id]);
}

function unfoldAll() {
  for (key in states) {
    if (states[key]) {
      fold(key);
    }
  }
}

function foldAll() {
  for (key in states) {
    if (!(states[key])) {
      fold(key);
    }
  }
}
