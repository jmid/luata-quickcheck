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
texts['fold000199'] = '<a href="javascript:fold(\'fold000199\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 199 to line 260</i>';
states['fold000199'] = false;
texts['fold000262'] = '<a href="javascript:fold(\'fold000262\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 262 to line 262</i>';
states['fold000262'] = false;
texts['fold000264'] = '<a href="javascript:fold(\'fold000264\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 264 to line 264</i>';
states['fold000264'] = false;
texts['fold000266'] = '<a href="javascript:fold(\'fold000266\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 266 to line 266</i>';
states['fold000266'] = false;
texts['fold000268'] = '<a href="javascript:fold(\'fold000268\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 268 to line 270</i>';
states['fold000268'] = false;
texts['fold000272'] = '<a href="javascript:fold(\'fold000272\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 272 to line 340</i>';
states['fold000272'] = false;
texts['fold000342'] = '<a href="javascript:fold(\'fold000342\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 342 to line 353</i>';
states['fold000342'] = false;
texts['fold000355'] = '<a href="javascript:fold(\'fold000355\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 355 to line 367</i>';
states['fold000355'] = false;
texts['fold000369'] = '<a href="javascript:fold(\'fold000369\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 369 to line 369</i>';
states['fold000369'] = false;
texts['fold000371'] = '<a href="javascript:fold(\'fold000371\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 371 to line 378</i>';
states['fold000371'] = false;

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
