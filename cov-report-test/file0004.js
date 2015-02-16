var texts = new Array();
var states = new Array();

texts['fold000001'] = '<a href="javascript:fold(\'fold000001\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1 to line 104</i>';
states['fold000001'] = false;
texts['fold000110'] = '<a href="javascript:fold(\'fold000110\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 110 to line 167</i>';
states['fold000110'] = false;
texts['fold000169'] = '<a href="javascript:fold(\'fold000169\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 169 to line 226</i>';
states['fold000169'] = false;
texts['fold000228'] = '<a href="javascript:fold(\'fold000228\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 228 to line 228</i>';
states['fold000228'] = false;
texts['fold000230'] = '<a href="javascript:fold(\'fold000230\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 230 to line 257</i>';
states['fold000230'] = false;
texts['fold000259'] = '<a href="javascript:fold(\'fold000259\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 259 to line 261</i>';
states['fold000259'] = false;
texts['fold000269'] = '<a href="javascript:fold(\'fold000269\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 269 to line 279</i>';
states['fold000269'] = false;
texts['fold000281'] = '<a href="javascript:fold(\'fold000281\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 281 to line 283</i>';
states['fold000281'] = false;
texts['fold000285'] = '<a href="javascript:fold(\'fold000285\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 285 to line 310</i>';
states['fold000285'] = false;
texts['fold000312'] = '<a href="javascript:fold(\'fold000312\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 312 to line 464</i>';
states['fold000312'] = false;

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
