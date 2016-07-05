var texts = new Array();
var states = new Array();

texts['fold000001'] = '<a href="javascript:fold(\'fold000001\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1 to line 4</i>';
states['fold000001'] = false;
texts['fold000008'] = '<a href="javascript:fold(\'fold000008\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 8 to line 10</i>';
states['fold000008'] = false;
texts['fold000014'] = '<a href="javascript:fold(\'fold000014\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 14 to line 16</i>';
states['fold000014'] = false;
texts['fold000020'] = '<a href="javascript:fold(\'fold000020\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 20 to line 20</i>';
states['fold000020'] = false;
texts['fold000022'] = '<a href="javascript:fold(\'fold000022\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 22 to line 22</i>';
states['fold000022'] = false;
texts['fold000026'] = '<a href="javascript:fold(\'fold000026\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 26 to line 27</i>';
states['fold000026'] = false;
texts['fold000029'] = '<a href="javascript:fold(\'fold000029\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 29 to line 29</i>';
states['fold000029'] = false;
texts['fold000032'] = '<a href="javascript:fold(\'fold000032\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 32 to line 35</i>';
states['fold000032'] = false;
texts['fold000045'] = '<a href="javascript:fold(\'fold000045\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 45 to line 48</i>';
states['fold000045'] = false;
texts['fold000050'] = '<a href="javascript:fold(\'fold000050\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 50 to line 50</i>';
states['fold000050'] = false;
texts['fold000054'] = '<a href="javascript:fold(\'fold000054\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 54 to line 55</i>';
states['fold000054'] = false;
texts['fold000057'] = '<a href="javascript:fold(\'fold000057\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 57 to line 57</i>';
states['fold000057'] = false;
texts['fold000061'] = '<a href="javascript:fold(\'fold000061\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 61 to line 62</i>';
states['fold000061'] = false;
texts['fold000071'] = '<a href="javascript:fold(\'fold000071\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 71 to line 72</i>';
states['fold000071'] = false;
texts['fold000075'] = '<a href="javascript:fold(\'fold000075\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 75 to line 75</i>';
states['fold000075'] = false;

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
