var texts = new Array();
var states = new Array();

texts['fold000001'] = '<a href="javascript:fold(\'fold000001\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1 to line 297</i>';
states['fold000001'] = false;
texts['fold000299'] = '<a href="javascript:fold(\'fold000299\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 299 to line 354</i>';
states['fold000299'] = false;
texts['fold000357'] = '<a href="javascript:fold(\'fold000357\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 357 to line 474</i>';
states['fold000357'] = false;
texts['fold000476'] = '<a href="javascript:fold(\'fold000476\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 476 to line 498</i>';
states['fold000476'] = false;
texts['fold000500'] = '<a href="javascript:fold(\'fold000500\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 500 to line 550</i>';
states['fold000500'] = false;
texts['fold000552'] = '<a href="javascript:fold(\'fold000552\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 552 to line 598</i>';
states['fold000552'] = false;
texts['fold000600'] = '<a href="javascript:fold(\'fold000600\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 600 to line 605</i>';
states['fold000600'] = false;
texts['fold000607'] = '<a href="javascript:fold(\'fold000607\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 607 to line 613</i>';
states['fold000607'] = false;
texts['fold000615'] = '<a href="javascript:fold(\'fold000615\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 615 to line 621</i>';
states['fold000615'] = false;
texts['fold000623'] = '<a href="javascript:fold(\'fold000623\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 623 to line 675</i>';
states['fold000623'] = false;
texts['fold000677'] = '<a href="javascript:fold(\'fold000677\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 677 to line 970</i>';
states['fold000677'] = false;
texts['fold000972'] = '<a href="javascript:fold(\'fold000972\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 972 to line 972</i>';
states['fold000972'] = false;
texts['fold000974'] = '<a href="javascript:fold(\'fold000974\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 974 to line 1100</i>';
states['fold000974'] = false;

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
