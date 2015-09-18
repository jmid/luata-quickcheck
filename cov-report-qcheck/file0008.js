var texts = new Array();
var states = new Array();

texts['fold000001'] = '<a href="javascript:fold(\'fold000001\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1 to line 143</i>';
states['fold000001'] = false;
texts['fold000146'] = '<a href="javascript:fold(\'fold000146\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 146 to line 146</i>';
states['fold000146'] = false;
texts['fold000148'] = '<a href="javascript:fold(\'fold000148\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 148 to line 314</i>';
states['fold000148'] = false;
texts['fold000316'] = '<a href="javascript:fold(\'fold000316\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 316 to line 397</i>';
states['fold000316'] = false;
texts['fold000399'] = '<a href="javascript:fold(\'fold000399\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 399 to line 399</i>';
states['fold000399'] = false;
texts['fold000403'] = '<a href="javascript:fold(\'fold000403\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 403 to line 404</i>';
states['fold000403'] = false;
texts['fold000406'] = '<a href="javascript:fold(\'fold000406\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 406 to line 406</i>';
states['fold000406'] = false;
texts['fold000412'] = '<a href="javascript:fold(\'fold000412\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 412 to line 451</i>';
states['fold000412'] = false;

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
