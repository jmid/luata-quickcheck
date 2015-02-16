var texts = new Array();
var states = new Array();

texts['fold000001'] = '<a href="javascript:fold(\'fold000001\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1 to line 56</i>';
states['fold000001'] = false;
texts['fold000059'] = '<a href="javascript:fold(\'fold000059\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 59 to line 59</i>';
states['fold000059'] = false;
texts['fold000063'] = '<a href="javascript:fold(\'fold000063\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 63 to line 63</i>';
states['fold000063'] = false;
texts['fold000066'] = '<a href="javascript:fold(\'fold000066\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 66 to line 101</i>';
states['fold000066'] = false;
texts['fold000103'] = '<a href="javascript:fold(\'fold000103\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 103 to line 128</i>';
states['fold000103'] = false;
texts['fold000131'] = '<a href="javascript:fold(\'fold000131\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 131 to line 131</i>';
states['fold000131'] = false;
texts['fold000133'] = '<a href="javascript:fold(\'fold000133\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 133 to line 144</i>';
states['fold000133'] = false;
texts['fold000147'] = '<a href="javascript:fold(\'fold000147\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 147 to line 190</i>';
states['fold000147'] = false;
texts['fold000196'] = '<a href="javascript:fold(\'fold000196\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 196 to line 215</i>';
states['fold000196'] = false;
texts['fold000217'] = '<a href="javascript:fold(\'fold000217\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 217 to line 219</i>';
states['fold000217'] = false;
texts['fold000223'] = '<a href="javascript:fold(\'fold000223\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 223 to line 223</i>';
states['fold000223'] = false;
texts['fold000225'] = '<a href="javascript:fold(\'fold000225\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 225 to line 225</i>';
states['fold000225'] = false;
texts['fold000228'] = '<a href="javascript:fold(\'fold000228\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 228 to line 228</i>';
states['fold000228'] = false;
texts['fold000230'] = '<a href="javascript:fold(\'fold000230\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 230 to line 230</i>';
states['fold000230'] = false;
texts['fold000234'] = '<a href="javascript:fold(\'fold000234\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 234 to line 234</i>';
states['fold000234'] = false;
texts['fold000237'] = '<a href="javascript:fold(\'fold000237\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 237 to line 237</i>';
states['fold000237'] = false;
texts['fold000239'] = '<a href="javascript:fold(\'fold000239\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 239 to line 286</i>';
states['fold000239'] = false;

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
