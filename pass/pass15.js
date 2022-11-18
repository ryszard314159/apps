#!/usr/bin/env node
'use strict';

const CHARS = {}
CHARS.digits = '0123456789'
CHARS.lower = 'abcdefghijklmnopqrstuvwxyz'
CHARS.upper = CHARS.lower.toUpperCase()
CHARS.punctuation = '!"#$%&\'()*+,-./:;<=>?@[\\]^_`{|}~'
const MP31 = 2**31 - 1 // Mersenne prime
// import assert from 'assert';s
const assert = require('assert');

/* permute chars in string using Durstenfeld shuffle algorithm */
function shuffle_string(string, gint=rig(MP31, seed)) {
  let z = string.split(''), i, j;
  for (i = z.length - 1; i > 0; i--) {
      j = gint.next().value % i;
      [z[i], z[j]] = [z[j], z[i]]
  }
  return z.join('')
}

function print(msg) {
  if (typeof window !== 'undefined') {
    document.write(msg + "<br>")
  } else {
    console.log(msg)
  }
}

function parse_cmd_line() {
  const { ArgumentParser,  ArgumentDefaultsHelpFormatter} = require('argparse');
  const parser = new ArgumentParser({
    description: 'Seeded password generator - generates password from hint and sekret \
    ; it is augmented by one special character (prefix), one lower and upper char and a digit',
    formatter_class: ArgumentDefaultsHelpFormatter
  });
  // parser.add_argument('-v', '--version', { action: 'version', version });
  parser.add_argument('-i', '--hint',
         { help: "provide non-empty string, otherwise random passwd will be generated  ", default: ''});
  parser.add_argument('-k', '--sekret', { help: 'hint augmentation', default: '0' });
  parser.add_argument('-s', '--salt', { help: 'hint augmentation', default: '0,1,2,3,4' });
  parser.add_argument('-L', '--len', { help: 'password length', type: 'int', default: 15});
  parser.add_argument('-u', '--unicode', { help: 'use ALL unicode chars', action: "store_true"});
  parser.add_argument('-r', '--letters', { help: 'use ascii letters', action: "store_true"});
  parser.add_argument('-t', '--digits', { help: 'use digits', action: "store_true"});
  parser.add_argument('-p', '--punctuation', { help: 'use punctuation', action: "store_true"});
  parser.add_argument('-x', '--prefix', { help: 'password prefix', default: '?' });
  parser.add_argument('-f', '--no-shuffle', { help: 'dont shuffle final string', action: "store_true"});
  parser.add_argument('-a', '--random', { help: 'create random password', action: "store_true"});
  parser.add_argument('-d', '--debug', { help: 'baz bar', action: "store_true"});
  parser.add_argument('-v', '--verbose', { help: 'add output', action: "store_true"});
  // let args =  parser.parse_args()
  // if (args.debug) console.dir(args);
  return parser
}

function hash_string(s) {
  assert(typeof(s) === 'string', `string expected, got ${s}`)
  const p = 2**5 - 1, m = MP31 // 2**31 - 1; // Mersenne primes: 2^(2, 3, 5, 7, 13, 17, 19, 31, 67, 127, 257)
  var value = MP31 // 2**31 - 1;
  var pp = 1;
  for (var j=0; j < s.length; j++) {
      value = (value + s.charCodeAt(j) * pp) % m;
      pp = (pp * p) % m;
  }
  return value;
}

/*
create small array of random integers
Donald Knuth MMIX: m = 2**64; a=6364136223846793005; c = 1442695040888963407
*/
// random integer generator
// generates integers in [0, max) interval
function* rig(max, hint) {
  let m = MP31, a=2147483629, c=2147483587;
  assert(typeof(max) === 'number' && (0 < max) && (max <= m),
    `rig: max (=${max}) must be number and has to be smaller than 2**31 - 1`)
  assert(typeof(hint) === 'string', `hint must be string, got ${typeof(hint)}`)
  var seed = hint == '' ? Date.now() : hash_string(hint)
  while (true) {
    seed = (a * seed + c) % m;
    yield Math.abs(seed % max)
  }
}

function get_random_string(n, charset='', gint=rig(MP31, '')) { // seed=undefined) {
  /*
  n       : length of string to generate
  charset : alphabet to use; NOTE: all unicode chars will be used if charset == ''
  gint    : random integer generator
  */
  const MAX_CODE_POINT = 1114111 + 1;
  assert(0 < n && n < 65 , `n must be in (0,65) range, got ${n}`)
  assert(charset.length < MAX_CODE_POINT, `too long charset, ${charset.length}`)
  /*
  see:
  https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/fromCharCode
  */
  let z = '';
  for (let i=0; i < n; i++) {
    let k = gint.next().value
    z += (charset !== '') ? charset[k % charset.length] : String.fromCodePoint(k % MAX_CODE_POINT)
  }
  return z
}

function getPass(args) {
  /*
  args.prefix      : prefix for generated password
  args.hint        : hint to generate password
  args.burn        : number of 'burn' steps in rng
  args.len         : length of password
  args.digits      : should digits be used?
  args.letters     : should letters be used?
  args.punctuation : should punctuation be used?
  */
  
  var charset = ''
  if (!args.unicode) {
    if (args.digits) charset += CHARS.digits
    if (args.letters) charset += CHARS.lower + CHARS.upper
    if (args.punctuation) charset += CHARS.punctuation
    if (charset.length == 0) charset = CHARS.digits + CHARS.lower + CHARS.upper
  }

  if (args.hint === '') { // generate and return random string from charset
    return get_random_string(args.len, charset)
  }

  let hint = args.hint + args.sekret
  /*
  add to prefix one lower, one upper character and one digit
  to satisfy requirements of many sites
  one or more special characters can be provided in args.prefix (default='?')
  */
  var salt = args.salt.split(',')
  assert(salt.length == 5, 'salt must be 5 chars')
  let lower = get_random_string(1, CHARS.lower,  rig(MP31, hint+salt[0]))
  let upper = get_random_string(1, CHARS.upper,  rig(MP31, hint+salt[1]))
  let digit = get_random_string(1, CHARS.digits, rig(MP31, hint+salt[2]))
  const prefix = args.prefix + lower + upper + digit
  let passwd = get_random_string(args.len - prefix.length, charset, rig(MP31, hint+salt[3]))
  passwd = prefix + passwd // augment password with prefix e.g. '?aZ9'
  if (!args.no_shuffle) {
    passwd = shuffle_string(passwd, rig(MP31, hint+salt[4]))
  }
  if (args.debug) {
    print("DEBUG: getPass: passwd=" + passwd)
    print("DEBUG: getPass: passwd.length=" + passwd.length)
    print("DEBUG: args.len=" + args.len)
    print("DEBUG: args.prefix=" + args.prefix)
    print("DEBUG: prefix=" + prefix + ' (augmented)')
    print("DEBUG: prefix.length=" + prefix.length)
    print("DEBUG: lower=" + lower)
    print("DEBUG: upper=" + upper)
    print("DEBUG: digit=" + digit)
    print("DEBUG: args.sekret=" + args.sekret)
    print("DEBUG: hint=" + hint)
    print("DEBUG: args.len=" + args.len)
    print("DEBUG: charset=" + charset)
  }
  if (typeof window === "undefined") {
    const CLIP = require('clipboardy');
    CLIP.writeSync(passwd)
  }
  return passwd
}

if (typeof window === 'undefined') {
  var parser = parse_cmd_line();
}
// const args = parser.parse_args(['--hint', 'aaa'])
const args = parser.parse_args()
if (args.debug) console.dir(args);
var passwd = getPass(args)
if (args.verbose) {
    print(`password >>> ${passwd} <<< (copied to clipboard)`)
}
// <script type="text/javascript" src="pass.js"></script>

/*
let MAX_CODE_POINT = 1114111
let v = new Set()
for (let j=0; j<MAX_CODE_POINT; j++) v.add(String.fromCodePoint(j))
*/
