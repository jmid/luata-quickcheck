var texts = new Array();
var states = new Array();

texts['fold000001'] = '<a href="javascript:fold(\'fold000001\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1 to line 30</i>';
states['fold000001'] = false;
texts['fold000032'] = '<a href="javascript:fold(\'fold000032\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 32 to line 59</i>';
states['fold000032'] = false;
texts['fold000061'] = '<a href="javascript:fold(\'fold000061\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 61 to line 76</i>';
states['fold000061'] = false;
texts['fold000078'] = '<a href="javascript:fold(\'fold000078\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 78 to line 164</i>';
states['fold000078'] = false;
texts['fold000166'] = '<a href="javascript:fold(\'fold000166\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 166 to line 167</i>';
states['fold000166'] = false;
texts['fold000169'] = '<a href="javascript:fold(\'fold000169\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 169 to line 237</i>';
states['fold000169'] = false;
texts['fold000239'] = '<a href="javascript:fold(\'fold000239\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 239 to line 286</i>';
states['fold000239'] = false;
texts['fold000288'] = '<a href="javascript:fold(\'fold000288\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 288 to line 368</i>';
states['fold000288'] = false;
texts['fold000370'] = '<a href="javascript:fold(\'fold000370\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 370 to line 382</i>';
states['fold000370'] = false;
texts['fold000384'] = '<a href="javascript:fold(\'fold000384\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 384 to line 388</i>';
states['fold000384'] = false;
texts['fold000390'] = '<a href="javascript:fold(\'fold000390\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 390 to line 549</i>';
states['fold000390'] = false;
texts['fold000551'] = '<a href="javascript:fold(\'fold000551\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 551 to line 560</i>';
states['fold000551'] = false;

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
