var texts = new Array();
var states = new Array();

texts['fold000001'] = '<a href="javascript:fold(\'fold000001\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1 to line 144</i>';
states['fold000001'] = false;
texts['fold000147'] = '<a href="javascript:fold(\'fold000147\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 147 to line 147</i>';
states['fold000147'] = false;
texts['fold000149'] = '<a href="javascript:fold(\'fold000149\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 149 to line 315</i>';
states['fold000149'] = false;
texts['fold000317'] = '<a href="javascript:fold(\'fold000317\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 317 to line 402</i>';
states['fold000317'] = false;
texts['fold000404'] = '<a href="javascript:fold(\'fold000404\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 404 to line 404</i>';
states['fold000404'] = false;
texts['fold000408'] = '<a href="javascript:fold(\'fold000408\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 408 to line 409</i>';
states['fold000408'] = false;
texts['fold000411'] = '<a href="javascript:fold(\'fold000411\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 411 to line 411</i>';
states['fold000411'] = false;
texts['fold000417'] = '<a href="javascript:fold(\'fold000417\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 417 to line 456</i>';
states['fold000417'] = false;

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
