
// KMP-ish subsequence finding for node.js

function l() {console.log.apply(console, arguments);}

function subseqenceof(subseq, haystack) {
  return subseqenceof1(subseq)(haystack);
}

function prefix(prefix, haystack) {
  if(prefix.length > haystack.length) return false;
  for(var i = 0; i < prefix.length; i++) {
    if (prefix[i] !== haystack[i]) return false;
  }
  return true;
}

function subseqenceof1(subseq) {
  
  var transitions = {};
  
  var final = subseq.length;
  
  for (var i = 0; i < subseq.length; i++) {
    transitions[i] = {};
    for (var k = i; k >= 0; k--) {
      // kinda naive
      if(prefix(subseq.slice(i-k,i),subseq)) {
        if(!transitions[i][subseq[k]]) {
          transitions[i][subseq[k]] = k+1;
        }
      }
    }
  }
  
  l(transitions);
  
  return function (haystack) {
    var state = 0;
    for (var i = 0; i < haystack.length; i++) {
      l(state, haystack[i])
      state = transitions[state][haystack[i]];
      if(state === undefined) state = 0;
      if(state === final) return i;
    }
    return false;
  }
}

l(subseqenceof([1,2,1,2,3],[4,5,6,4,3,3,6,7,1,2,1,1,1,1,2,3,6,3,2,4,5,1,2,1,2,1,2,6,3,2,5,7,1,3,3,2,3,5,7,6,2,1,2,1,2,3,5,7,8,4,8]));
