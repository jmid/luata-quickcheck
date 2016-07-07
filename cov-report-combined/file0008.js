var texts = new Array();
var states = new Array();

texts['fold000001'] = '<a href="javascript:fold(\'fold000001\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1 to line 30</i>';
states['fold000001'] = false;
texts['fold000032'] = '<a href="javascript:fold(\'fold000032\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 32 to line 57</i>';
states['fold000032'] = false;
texts['fold000059'] = '<a href="javascript:fold(\'fold000059\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 59 to line 73</i>';
states['fold000059'] = false;
texts['fold000075'] = '<a href="javascript:fold(\'fold000075\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 75 to line 156</i>';
states['fold000075'] = false;
texts['fold000158'] = '<a href="javascript:fold(\'fold000158\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 158 to line 159</i>';
states['fold000158'] = false;
texts['fold000161'] = '<a href="javascript:fold(\'fold000161\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 161 to line 232</i>';
states['fold000161'] = false;
texts['fold000234'] = '<a href="javascript:fold(\'fold000234\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 234 to line 284</i>';
states['fold000234'] = false;
texts['fold000286'] = '<a href="javascript:fold(\'fold000286\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 286 to line 362</i>';
states['fold000286'] = false;
texts['fold000364'] = '<a href="javascript:fold(\'fold000364\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 364 to line 370</i>';
states['fold000364'] = false;
texts['fold000372'] = '<a href="javascript:fold(\'fold000372\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 372 to line 376</i>';
states['fold000372'] = false;
texts['fold000378'] = '<a href="javascript:fold(\'fold000378\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 378 to line 538</i>';
states['fold000378'] = false;
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
