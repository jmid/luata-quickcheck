var texts = new Array();
var states = new Array();

texts['fold000001'] = '<a href="javascript:fold(\'fold000001\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1 to line 56</i>';
states['fold000001'] = false;
texts['fold000059'] = '<a href="javascript:fold(\'fold000059\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 59 to line 59</i>';
states['fold000059'] = false;
texts['fold000062'] = '<a href="javascript:fold(\'fold000062\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 62 to line 62</i>';
states['fold000062'] = false;
texts['fold000065'] = '<a href="javascript:fold(\'fold000065\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 65 to line 65</i>';
states['fold000065'] = false;
texts['fold000068'] = '<a href="javascript:fold(\'fold000068\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 68 to line 244</i>';
states['fold000068'] = false;
texts['fold000247'] = '<a href="javascript:fold(\'fold000247\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 247 to line 324</i>';
states['fold000247'] = false;

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
