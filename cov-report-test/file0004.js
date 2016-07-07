var texts = new Array();
var states = new Array();

texts['fold000001'] = '<a href="javascript:fold(\'fold000001\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1 to line 232</i>';
states['fold000001'] = false;
texts['fold000234'] = '<a href="javascript:fold(\'fold000234\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 234 to line 262</i>';
states['fold000234'] = false;
texts['fold000264'] = '<a href="javascript:fold(\'fold000264\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 264 to line 269</i>';
states['fold000264'] = false;
texts['fold000271'] = '<a href="javascript:fold(\'fold000271\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 271 to line 284</i>';
states['fold000271'] = false;
texts['fold000286'] = '<a href="javascript:fold(\'fold000286\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 286 to line 288</i>';
states['fold000286'] = false;
texts['fold000290'] = '<a href="javascript:fold(\'fold000290\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 290 to line 315</i>';
states['fold000290'] = false;
texts['fold000317'] = '<a href="javascript:fold(\'fold000317\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 317 to line 469</i>';
states['fold000317'] = false;

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
