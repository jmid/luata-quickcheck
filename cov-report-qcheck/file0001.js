var texts = new Array();
var states = new Array();

texts['fold000001'] = '<a href="javascript:fold(\'fold000001\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1 to line 5</i>';
states['fold000001'] = false;
texts['fold000007'] = '<a href="javascript:fold(\'fold000007\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 7 to line 49</i>';
states['fold000007'] = false;
texts['fold000053'] = '<a href="javascript:fold(\'fold000053\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 53 to line 186</i>';
states['fold000053'] = false;
texts['fold000188'] = '<a href="javascript:fold(\'fold000188\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 188 to line 197</i>';
states['fold000188'] = false;
texts['fold000199'] = '<a href="javascript:fold(\'fold000199\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 199 to line 241</i>';
states['fold000199'] = false;
texts['fold000244'] = '<a href="javascript:fold(\'fold000244\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 244 to line 244</i>';
states['fold000244'] = false;
texts['fold000246'] = '<a href="javascript:fold(\'fold000246\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 246 to line 270</i>';
states['fold000246'] = false;
texts['fold000272'] = '<a href="javascript:fold(\'fold000272\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 272 to line 272</i>';
states['fold000272'] = false;
texts['fold000274'] = '<a href="javascript:fold(\'fold000274\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 274 to line 274</i>';
states['fold000274'] = false;
texts['fold000276'] = '<a href="javascript:fold(\'fold000276\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 276 to line 276</i>';
states['fold000276'] = false;
texts['fold000278'] = '<a href="javascript:fold(\'fold000278\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 278 to line 279</i>';
states['fold000278'] = false;
texts['fold000281'] = '<a href="javascript:fold(\'fold000281\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 281 to line 283</i>';
states['fold000281'] = false;
texts['fold000285'] = '<a href="javascript:fold(\'fold000285\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 285 to line 353</i>';
states['fold000285'] = false;
texts['fold000355'] = '<a href="javascript:fold(\'fold000355\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 355 to line 366</i>';
states['fold000355'] = false;
texts['fold000368'] = '<a href="javascript:fold(\'fold000368\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 368 to line 380</i>';
states['fold000368'] = false;
texts['fold000382'] = '<a href="javascript:fold(\'fold000382\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 382 to line 382</i>';
states['fold000382'] = false;
texts['fold000384'] = '<a href="javascript:fold(\'fold000384\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 384 to line 391</i>';
states['fold000384'] = false;

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
