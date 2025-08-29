;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024 Evgenii Lepikhin <johnlepikhin@gmail.com>
;;;
;;; This file is not part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (johnlepikhin packages rust-crates)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix build-system cargo) #:select (crate-source define-cargo-inputs))
  #:export (lookup-cargo-inputs))

;; Crate sources for i3im dependencies
(define rust-aho-corasick-1.1.3
  (crate-source "aho-corasick" "1.1.3"
                "05mrpkvdgp5d20y2p989f187ry9diliijgwrs254fs9s1m1x6q4f"))

(define rust-anstream-0.6.18
  (crate-source "anstream" "0.6.18"
                "16sjk4x3ns2c3ya1x28a44kh6p47c7vhk27251i015hik1lm7k4a"))

(define rust-anstyle-1.0.10
  (crate-source "anstyle" "1.0.10"
                "1yai2vppmd7zlvlrp9grwll60knrmscalf8l2qpfz8b7y5lkpk2m"))

(define rust-anstyle-parse-0.2.6
  (crate-source "anstyle-parse" "0.2.6"
                "1acqayy22fwzsrvr6n0lz6a4zvjjcvgr5sm941m7m0b2fr81cb9v"))

(define rust-anstyle-query-1.1.2
  (crate-source "anstyle-query" "1.1.2"
                "036nm3lkyk43xbps1yql3583fp4hg3b1600is7mcyxs1gzrpm53r"))

(define rust-anstyle-wincon-3.0.6
  (crate-source "anstyle-wincon" "3.0.6"
                "099ir0w3lbpsp1nxdzbf4anq98ww8ykyc9pd1g03xgkj1v7dn291"))

(define rust-anyhow-1.0.95
  (crate-source "anyhow" "1.0.95"
                "010vd1ki8w84dzgx6c81sc8qm9n02fxic1gkpv52zp4nwrn0kb1l"))

(define rust-arc-swap-1.7.1
  (crate-source "arc-swap" "1.7.1"
                "0mrl9a9r9p9bln74q6aszvf22q1ijiw089jkrmabfqkbj31zixv9"))

(define rust-bitflags-1.3.2
  (crate-source "bitflags" "1.3.2"
                "12ki6w8gn1ldq7yz9y680llwk5gmrhrzszaa17g1sbrw2r2qvwxy"))

(define rust-bitflags-2.6.0
  (crate-source "bitflags" "2.6.0"
                "1pkidwzn3hnxlsl8zizh0bncgbjnw7c41cx7bby26ncbzmiznj5h"))

(define rust-byteorder-1.5.0
  (crate-source "byteorder" "1.5.0"
                "0jzncxyf404mwqdbspihyzpkndfgda450l0893pz5xj685cg5l0z"))

(define rust-cfg-if-1.0.0
  (crate-source "cfg-if" "1.0.0"
                "1za0vb97n4brpzpv8lsbnzmq5r8f2b0cpqqr0sy8h5bn751xxwds"))

(define rust-clap-4.5.23
  (crate-source "clap" "4.5.23"
                "110cf0i9fmkfqzqhi1h8za9y0vnr5rwhy3wmv1p0rcgp5vnffd9i"))

(define rust-clap-builder-4.5.23
  (crate-source "clap_builder" "4.5.23"
                "0f28rgc09kdgfq1hgg1bb1ydaw243w6dwyw74syz439k6b32yn1h"))

(define rust-clap-derive-4.5.18
  (crate-source "clap_derive" "4.5.18"
                "1ardb26bvcpg72q9myr7yir3a8c83gx7vxk1cccabsd9n73s1ija"))

(define rust-clap-lex-0.7.4
  (crate-source "clap_lex" "0.7.4"
                "19nwfls5db269js5n822vkc8dw0wjq2h1wf0hgr06ld2g52d2spl"))

(define rust-colorchoice-1.0.3
  (crate-source "colorchoice" "1.0.3"
                "1439m3r3jy3xqck8aa13q658visn71ki76qa93cy55wkmalwlqsv"))

(define rust-crossbeam-channel-0.5.14
  (crate-source "crossbeam-channel" "0.5.14"
                "0wa41qybq5w8s70anb472myh4fid4aw6v65vws6wn528w9l6vfh6"))

(define rust-crossbeam-utils-0.8.21
  (crate-source "crossbeam-utils" "0.8.21"
                "0a3aa2bmc8q35fb67432w16wvi54sfmb69rk9h5bhd18vw0c99fh"))

(define rust-deranged-0.3.11
  (crate-source "deranged" "0.3.11"
                "1d1ibqqnr5qdrpw8rclwrf1myn3wf0dygl04idf4j2s49ah6yaxl"))

(define rust-dirs-5.0.1
  (crate-source "dirs" "5.0.1"
                "0992xk5vx75b2x91nw9ssb51mpl8x73j9rxmpi96cryn0ffmmi24"))

(define rust-dirs-next-2.0.0
  (crate-source "dirs-next" "2.0.0"
                "1q9kr151h9681wwp6is18750ssghz6j9j7qm7qi1ngcwy7mzi35r"))

(define rust-dirs-sys-0.4.1
  (crate-source "dirs-sys" "0.4.1"
                "071jy0pvaad9lsa6mzawxrh7cmr7hsmsdxwzm7jzldfkrfjha3sj"))

(define rust-dirs-sys-next-0.1.2
  (crate-source "dirs-sys-next" "0.1.2"
                "0kavhavdxv4phzj4l0psvh55hszwnr0rcz8sxbvx20pyqi2a3gaf"))

(define rust-either-1.13.0
  (crate-source "either" "1.13.0"
                "1w2c1mybrd7vljyxk77y9f4w9dyjrmp3yp82mk7bcm8848fazcb0"))

(define rust-equivalent-1.0.1
  (crate-source "equivalent" "1.0.1"
                "1malmx5f4lkfvqasz319lq6gb3ddg19yzf9s8cykfsgzdmyq0hsl"))

(define rust-error-chain-0.12.4
  (crate-source "error-chain" "0.12.4"
                "1z6y5isg0il93jp287sv7pn10i4wrkik2cpyk376wl61rawhcbrd"))

(define rust-getrandom-0.2.15
  (crate-source "getrandom" "0.2.15"
                "1mzlnrb3dgyd1fb84gvw10pyr8wdqdl4ry4sr64i1s8an66pqmn4"))

(define rust-hashbrown-0.15.2
  (crate-source "hashbrown" "0.15.2"
                "12dj0yfn59p3kh3679ac0w1fagvzf4z2zp87a13gbbqbzw0185dz"))

(define rust-heck-0.3.3
  (crate-source "heck" "0.3.3"
                "0b0kkr790p66lvzn9nsmfjvydrbmh9z5gb664jchwgw64vxiwqkd"))

(define rust-heck-0.5.0
  (crate-source "heck" "0.5.0"
                "1sjmpsdl8czyh9ywl3qcsfsq9a307dg4ni2vnlwgnzzqhc4y0113"))

(define rust-hermit-abi-0.1.18
  (crate-source "hermit-abi" "0.1.18"
                "0p6czgbk1izviwxzm6ypy3vz2wqj1yd3ab03wp82xqjng7klsbrj"))

(define rust-hermit-abi-0.4.0
  (crate-source "hermit-abi" "0.4.0"
                "1k1zwllx6nfq417hy38x4akw1ivlv68ymvnzyxs76ffgsqcskxpv"))

(define rust-i3ipc-jl-0.11.2
  (crate-source "i3ipc-jl" "0.11.2"
                "1lc4gyb2br8dy4crv3drn85pssmbykb9rwfpkzipglh74g0b97ql"))

(define rust-indexmap-2.7.0
  (crate-source "indexmap" "2.7.0"
                "07s7jmdymvd0rm4yswp0j3napx57hkjm9gs9n55lvs2g78vj5y32"))

(define rust-is-terminal-0.4.13
  (crate-source "is-terminal" "0.4.13"
                "0jwgjjz33kkmnwai3nsdk1pz9vb6gkqvw1d1vq7bs3q48kinh7r6"))

(define rust-is-terminal-polyfill-1.70.1
  (crate-source "is_terminal_polyfill" "1.70.1"
                "1kwfgglh91z33kl0w5i338mfpa3zs0hidq5j4ny4rmjwrikchhvr"))

(define rust-itertools-0.8.2
  (crate-source "itertools" "0.8.2"
                "1154j48aw913v5jnyhpxialxhdn2sfpl4d7bwididyb1r05jsspm"))

(define rust-itoa-1.0.14
  (crate-source "itoa" "1.0.14"
                "0x26kr9m062mafaxgcf2p6h2x7cmixm0zw95aipzn2hr3d5jlnnp"))

(define rust-lazy-static-1.5.0
  (crate-source "lazy_static" "1.5.0"
                "1zk6dqqni0193xg6iijh7i3i44sryglwgvx20spdvwk3r6sbrlmv"))

(define rust-libc-0.2.169
  (crate-source "libc" "0.2.169"
                "02m253hs8gw0m1n8iyrsc4n15yzbqwhddi7w1l0ds7i92kdsiaxm"))

(define rust-libredox-0.1.3
  (crate-source "libredox" "0.1.3"
                "139602gzgs0k91zb7dvgj1qh4ynb8g1lbxsswdim18hcb6ykgzy0"))

(define rust-log-0.4.22
  (crate-source "log" "0.4.22"
                "093vs0wkm1rgyykk7fjbqp2lwizbixac1w52gv109p5r4jh0p9x7"))

(define rust-memchr-2.7.4
  (crate-source "memchr" "2.7.4"
                "18z32bhxrax0fnjikv475z7ii718hq457qwmaryixfxsl2qrmjkq"))

(define rust-num-conv-0.1.0
  (crate-source "num-conv" "0.1.0"
                "1ndiyg82q73783jq18isi71a7mjh56wxrk52rlvyx0mi5z9ibmai"))

(define rust-once-cell-1.20.2
  (crate-source "once_cell" "1.20.2"
                "0xb7rw1aqr7pa4z3b00y7786gyf8awx2gca3md73afy76dzgwq8j"))

(define rust-option-ext-0.2.0
  (crate-source "option-ext" "0.2.0"
                "0zbf7cx8ib99frnlanpyikm1bx8qn8x602sw1n7bg6p9x94lyx04"))

(define rust-powerfmt-0.2.0
  (crate-source "powerfmt" "0.2.0"
                "14ckj2xdpkhv3h6l5sdmb9f1d57z8hbfpdldjc2vl5givq2y77j3"))

(define rust-proc-macro2-1.0.92
  (crate-source "proc-macro2" "1.0.92"
                "1c1vjy5wg8iy7kxsxda564qf4ljp0asysmbn2i7caj177x5m9lrp"))

(define rust-quote-1.0.37
  (crate-source "quote" "1.0.37"
                "1brklraw2g34bxy9y4q1nbrccn7bv36ylihv12c9vlcii55x7fdm"))

(define rust-redox-users-0.4.6
  (crate-source "redox_users" "0.4.6"
                "0hya2cxx6hxmjfxzv9n8rjl5igpychav7zfi1f81pz6i4krry05s"))

(define rust-regex-1.11.1
  (crate-source "regex" "1.11.1"
                "148i41mzbx8bmq32hsj1q4karkzzx5m60qza6gdw4pdc9qdyyi5m"))

(define rust-regex-automata-0.4.9
  (crate-source "regex-automata" "0.4.9"
                "02092l8zfh3vkmk47yjc8d631zhhcd49ck2zr133prvd3z38v7l0"))

(define rust-regex-syntax-0.8.5
  (crate-source "regex-syntax" "0.8.5"
                "0p41p3hj9ww7blnbwbj9h7rwxzxg0c1hvrdycgys8rxyhqqw859b"))

(define rust-rustversion-1.0.18
  (crate-source "rustversion" "1.0.18"
                "0j2207vmgrcxwwwvknfn3lwv4i8djhjnxlvwdnz8bwijqqmrz08f"))

(define rust-ryu-1.0.18
  (crate-source "ryu" "1.0.18"
                "17xx2s8j1lln7iackzd9p0sv546vjq71i779gphjq923vjh5pjzk"))

(define rust-serde-1.0.216
  (crate-source "serde" "1.0.216"
                "13ikqs0cvd220530x4rj1m9ab5wcflrwkw7cpvl9fnlkdq0q35qb"))

(define rust-serde-derive-1.0.216
  (crate-source "serde_derive" "1.0.216"
                "0pm5bm4354n40ir12bbs829arlqwjrw0wmzd4xk5r1kkpzdmky26"))

(define rust-serde-json-1.0.134
  (crate-source "serde_json" "1.0.134"
                "0z8wk61rzpqjmnwhv6k9zikhsfmsb6lr6qbg84aqpr1fqisl23yh"))

(define rust-serde-regex-1.1.0
  (crate-source "serde_regex" "1.1.0"
                "1pxsnxb8c198szghk1hvzvhva36w2q5zs70hqkmdf5d89qd6y4x8"))

(define rust-serde-yaml-0.9.34+deprecated
  (crate-source "serde_yaml" "0.9.34+deprecated"
                "0isba1fjyg3l6rxk156k600ilzr8fp7crv82rhal0rxz5qd1m2va"))

(define rust-shellexpand-3.1.0
  (crate-source "shellexpand" "3.1.0"
                "0jz1i14ziz8gbyj71212s7dqrw6q96f25i48zkmy66fcjhxzl0ys"))

(define rust-slog-2.7.0
  (crate-source "slog" "2.7.0"
                "01ldk4yarx7x4y4rgsf4kmrcy3wrpcxdd53v2lkk355x9rnh8iw3"))

(define rust-slog-async-2.8.0
  (crate-source "slog-async" "2.8.0"
                "113b17aw7jx7mr68vwfq2yiv6mb4702hz6a0g587jb4ai67h7j3j"))

(define rust-slog-envlogger-2.2.0
  (crate-source "slog-envlogger" "2.2.0"
                "1h7m0jnj6kvsn9553fyqvaw3swy3pwpmwamqyhnnkv9zqh5ilslh"))

(define rust-slog-scope-4.4.0
  (crate-source "slog-scope" "4.4.0"
                "11n7nd0g3iab8ahcwnxzpmchi4ycgjsq5nj9jn3d4k17qfsa959g"))

(define rust-slog-stdlog-4.1.1
  (crate-source "slog-stdlog" "4.1.1"
                "0gpsf62ckblpc6a70dnhsz677c7s5cz4glpqsf8p5bmvwnnb41k7"))

(define rust-slog-syslog-jl-0.13.1
  (crate-source "slog-syslog-jl" "0.13.1"
                "09hlvi4l1627fsmfxaclw0m7c2if40n7mmqrjrsxmbxgln0kb90r"))

(define rust-slog-term-2.9.1
  (crate-source "slog-term" "2.9.1"
                "1s0h8qhqnvy5a7m7gmnca2a2d5m5a4sz1hc26xfgxawqp7825q5n"))

(define rust-strsim-0.11.1
  (crate-source "strsim" "0.11.1"
                "0kzvqlw8hxqb7y598w1s0hxlnmi84sg5vsipp3yg5na5d1rvba3x"))

(define rust-structdoc-0.1.4
  (crate-source "structdoc" "0.1.4"
                "04bzjwlg8cxfbqgmg2i3s5y0lgmcsdj173byix2sa3dlf6955n4g"))

(define rust-structdoc-derive-0.1.4
  (crate-source "structdoc-derive" "0.1.4"
                "1yjdi987jaqbypfanyllldk6ww2vswniniavn3pb4zrpazc75ah1"))

(define rust-syn-1.0.109
  (crate-source "syn" "1.0.109"
                "0ds2if4600bd59wsv7jjgfkayfzy3hnazs394kz6zdkmna8l3dkj"))

(define rust-syn-2.0.91
  (crate-source "syn" "2.0.91"
                "0df08gvjksnnapcqcbc72l44jlr1r957y7cbhnvk7ga3lasvqg6m"))

(define rust-syslog-5.0.0
  (crate-source "syslog" "5.0.0"
                "0paii62qnwjnfliygdal1x3hqxjkci1nlczfydv7kh3rnvqqwpcs"))

(define rust-take-mut-0.2.2
  (crate-source "take_mut" "0.2.2"
                "0q2d7w6nd5bl7bay5csq065sjg8fw0jcx6hl1983cpzf25fh0r7p"))

(define rust-term-0.7.0
  (crate-source "term" "0.7.0"
                "07xzxmg7dbhlirpyfq09v7cfb9gxn0077sqqvszgjvyrjnngi7f5"))

(define rust-thiserror-1.0.69
  (crate-source "thiserror" "1.0.69"
                "0lizjay08agcr5hs9yfzzj6axs53a2rgx070a1dsi3jpkcrzbamn"))

(define rust-thiserror-impl-1.0.69
  (crate-source "thiserror-impl" "1.0.69"
                "1h84fmn2nai41cxbhk6pqf46bxqq1b344v8yz089w1chzi76rvjg"))

(define rust-thread-local-1.1.8
  (crate-source "thread_local" "1.1.8"
                "173i5lyjh011gsimk21np9jn8al18rxsrkjli20a7b8ks2xgk7lb"))

(define rust-time-0.1.45
  (crate-source "time" "0.1.45"
                "0nl0pzv9yf56djy8y5dx25nka5pr2q1ivlandb3d24pksgx7ly8v"))

(define rust-time-0.3.37
  (crate-source "time" "0.3.37"
                "08bvydyc14plkwhchzia5bcdbmm0mk5fzilsdpjx06w6hf48drrm"))

(define rust-time-core-0.1.2
  (crate-source "time-core" "0.1.2"
                "1wx3qizcihw6z151hywfzzyd1y5dl804ydyxci6qm07vbakpr4pg"))

(define rust-time-macros-0.2.19
  (crate-source "time-macros" "0.2.19"
                "1pl558z26pp342l5y91n6dxb60xwhar975wk6jc4npiygq0ycd18"))

(define rust-unicode-ident-1.0.14
  (crate-source "unicode-ident" "1.0.14"
                "10ywa1pg0glgkr4l3dppjxizr9r2b7im0ycbfa0137l69z5fdfdd"))

(define rust-unicode-segmentation-1.12.0
  (crate-source "unicode-segmentation" "1.12.0"
                "14qla2jfx74yyb9ds3d2mpwpa4l4lzb9z57c6d2ba511458z5k7n"))

(define rust-unindent-0.1.11
  (crate-source "unindent" "0.1.11"
                "171may3v15wzc10z64i8sahdz49d031v7424mjsifa205ml6sxp1"))

(define rust-unsafe-libyaml-0.2.11
  (crate-source "unsafe-libyaml" "0.2.11"
                "0qdq69ffl3v5pzx9kzxbghzn0fzn266i1xn70y88maybz9csqfk7"))

(define rust-utf8parse-0.2.2
  (crate-source "utf8parse" "0.2.2"
                "088807qwjq46azicqwbhlmzwrbkz7l4hpw43sdkdyyk524vdxaq6"))

(define rust-version-check-0.9.5
  (crate-source "version_check" "0.9.5"
                "0nhhi4i5x89gm911azqbn7avs9mdacw2i3vcz3cnmz3mv4rqz4hb"))

(define rust-wasi-0.10.0+wasi-snapshot-preview1
  (crate-source "wasi" "0.10.0+wasi-snapshot-preview1"
                "07y3l8mzfzzz4cj09c8y90yak4hpsi9g7pllyzpr6xvwrabka50s"))

(define rust-wasi-0.11.0+wasi-snapshot-preview1
  (crate-source "wasi" "0.11.0+wasi-snapshot-preview1"
                "08z4hxwkpdpalxjps1ai9y7ihin26y9f476i53dv98v45gkqg3cw"))

(define rust-winapi-0.3.9
  (crate-source "winapi" "0.3.9"
                "06gl025x418lchw1wxj64ycr7gha83m44cjr5sarhynd9xkrm0sw"))

(define rust-winapi-i686-pc-windows-gnu-0.4.0
  (crate-source "winapi-i686-pc-windows-gnu" "0.4.0"
                "1dmpa6mvcvzz16zg6d5vrfy4bxgg541wxrcip7cnshi06v38ffxc"))

(define rust-winapi-x86-64-pc-windows-gnu-0.4.0
  (crate-source "winapi-x86_64-pc-windows-gnu" "0.4.0"
                "0gqq64czqb64kskjryj8isp62m2sgvx25yyj3kpc2myh85w24bki"))

(define rust-winapi-x86-64-pc-windows-gnu-0.4.0
  (crate-source "winapi-x86_64-pc-windows-gnu" "0.4.0"
                "0gqq64czqb64kskjryj8isp62m2sgvx25yyj3kpc2myh85w24bki"))

(define rust-windows-sys-0.48.0
  (crate-source "windows-sys" "0.48.0"
                "1aan23v5gs7gya1lc46hqn9mdh8yph3fhxmhxlw36pn6pqc28zb7"))

(define rust-windows-sys-0.52.0
  (crate-source "windows-sys" "0.52.0"
                "0gd3v4ji88490zgb6b5mq5zgbvwv7zx1ibn8v3x83rwcdbryaar8"))

(define rust-windows-sys-0.59.0
  (crate-source "windows-sys" "0.59.0"
                "0fw5672ziw8b3zpmnbp9pdv1famk74f1l9fcbc3zsrzdg56vqf0y"))

(define rust-windows-targets-0.48.5
  (crate-source "windows-targets" "0.48.5"
                "034ljxqshifs1lan89xwpcy1hp0lhdh4b5n0d2z4fwjx2piacbws"))

(define rust-windows-targets-0.52.6
  (crate-source "windows-targets" "0.52.6"
                "0wwrx625nwlfp7k93r2rra568gad1mwd888h1jwnl0vfg5r4ywlv"))

(define rust-windows-aarch64-gnullvm-0.48.5
  (crate-source "windows_aarch64_gnullvm" "0.48.5"
                "1n05v7qblg1ci3i567inc7xrkmywczxrs1z3lj3rkkxw18py6f1b"))

(define rust-windows-aarch64-gnullvm-0.52.6
  (crate-source "windows_aarch64_gnullvm" "0.52.6"
                "1lrcq38cr2arvmz19v32qaggvj8bh1640mdm9c2fr877h0hn591j"))

(define rust-windows-aarch64-msvc-0.48.5
  (crate-source "windows_aarch64_msvc" "0.48.5"
                "1g5l4ry968p73g6bg6jgyvy9lb8fyhcs54067yzxpcpkf44k2dfw"))

(define rust-windows-aarch64-msvc-0.52.6
  (crate-source "windows_aarch64_msvc" "0.52.6"
                "0sfl0nysnz32yyfh773hpi49b1q700ah6y7sacmjbqjjn5xjmv09"))

(define rust-windows-i686-gnu-0.48.5
  (crate-source "windows_i686_gnu" "0.48.5"
                "0gklnglwd9ilqx7ac3cn8hbhkraqisd0n83jxzf9837nvvkiand7"))

(define rust-windows-i686-gnu-0.52.6
  (crate-source "windows_i686_gnu" "0.52.6"
                "02zspglbykh1jh9pi7gn8g1f97jh1rrccni9ivmrfbl0mgamm6wf"))

(define rust-windows-i686-gnullvm-0.52.6
  (crate-source "windows_i686_gnullvm" "0.52.6"
                "0rpdx1537mw6slcpqa0rm3qixmsb79nbhqy5fsm3q2q9ik9m5vhf"))

(define rust-windows-i686-msvc-0.48.5
  (crate-source "windows_i686_msvc" "0.48.5"
                "01m4rik437dl9rdf0ndnm2syh10hizvq0dajdkv2fjqcywrw4mcg"))

(define rust-windows-i686-msvc-0.52.6
  (crate-source "windows_i686_msvc" "0.52.6"
                "0rkcqmp4zzmfvrrrx01260q3xkpzi6fzi2x2pgdcdry50ny4h294"))

(define rust-windows-x86-64-gnu-0.48.5
  (crate-source "windows_x86_64_gnu" "0.48.5"
                "13kiqqcvz2vnyxzydjh73hwgigsdr2z1xpzx313kxll34nyhmm2k"))

(define rust-windows-x86-64-gnu-0.52.6
  (crate-source "windows_x86_64_gnu" "0.52.6"
                "0y0sifqcb56a56mvn7xjgs8g43p33mfqkd8wj1yhrgxzma05qyhl"))

(define rust-windows-x86-64-gnullvm-0.48.5
  (crate-source "windows_x86_64_gnullvm" "0.48.5"
                "1k24810wfbgz8k48c2yknqjmiigmql6kk3knmddkv8k8g1v54yqb"))

(define rust-windows-x86-64-gnullvm-0.52.6
  (crate-source "windows_x86_64_gnullvm" "0.52.6"
                "03gda7zjx1qh8k9nnlgb7m3w3s1xkysg55hkd1wjch8pqhyv5m94"))

(define rust-windows-x86-64-msvc-0.48.5
  (crate-source "windows_x86_64_msvc" "0.48.5"
                "0f4mdp895kkjh9zv8dxvn4pc10xr7839lf5pa9l0193i2pkgr57d"))

(define rust-windows-x86-64-msvc-0.52.6
  (crate-source "windows_x86_64_msvc" "0.52.6"
                "1v7rb5cibyzx8vak29pdrk8nx9hycsjs4w0jgms08qk49jl6v7sq"))

;; Additional crates for xidlehook
(define rust-ansi-term-0.11.0
  (crate-source "ansi_term" "0.11.0"
                "16wpvrghvd0353584i1idnsgm0r3vchg8fyrm0x8ayv1rgvbljgf"))

(define rust-atty-0.2.14
  (crate-source "atty" "0.2.14"
                "1s7yslcs6a28c5vz7jwj63lkfgyx8mx99fdirlhi9lbhhzhrpcyr"))

(define rust-autocfg-1.0.1
  (crate-source "autocfg" "1.0.1"
                "0jj6i9zn4gjl03kjvziqdji6rwx8ykz8zk2ngpc331z2g3fk3c6d"))

(define rust-bytes-0.5.6
  (crate-source "bytes" "0.5.6"
                "0f5s7xq6qzmdh22ygsy8v0sp02m51y0radvq4i4y8cizy1lfqk0f"))

(define rust-cc-1.0.67
  (crate-source "cc" "1.0.67"
                "1z9p27ys80shv09zhknmlal8jjra78agdwg97i6jjd6lg83rpip3"))

(define rust-clap-2.33.3
  (crate-source "clap" "2.33.3"
                "00i065a58987k1sbzqmlz721rw521zcg08jmsh40gi3khp3qmr9p"))

(define rust-env-logger-0.7.1
  (crate-source "env_logger" "0.7.1"
                "0djx8h8xfib43g5w94r1m1mkky5spcw4wblzgnhiyg5vnfxknls4"))

(define rust-fnv-1.0.7
  (crate-source "fnv" "1.0.7"
                "1hc2mcqha06aibcaza94vbi81j6pr9a1bbxrxjfhc91zin8yr7iz"))

(define rust-fuchsia-zircon-0.3.3
  (crate-source "fuchsia-zircon" "0.3.3"
                "10jxc5ks1x06gpd0xg51kcjrxr35nj6qhx2zlc5n7bmskv3675rf"))

(define rust-fuchsia-zircon-sys-0.3.3
  (crate-source "fuchsia-zircon-sys" "0.3.3"
                "19zp2085qsyq2bh1gvcxq1lb8w6v6jj9kbdkhpdjrl95fypakjix"))

(define rust-futures-0.3.13
  (crate-source "futures" "0.3.13"
                "1h8lx9iz3k43lwr63gn49nsfph09qkvnlh3nm0xmj78i35rncmbz"))

(define rust-futures-channel-0.3.13
  (crate-source "futures-channel" "0.3.13"
                "0f99mnrg7p66gn0f4zwkh53ri0cc7s7rsb4wnsddnmwvhggx4bcc"))

(define rust-futures-core-0.3.13
  (crate-source "futures-core" "0.3.13"
                "151c26lzgmk3y67bl407vlhr4hl76ydaa4fzqfyn43mzz9r6lj8m"))

(define rust-futures-executor-0.3.13
  (crate-source "futures-executor" "0.3.13"
                "1l8akjf64yf8lbm4q49ccspfwpfn5mikfajb1105jkfqjrxln6l9"))

(define rust-futures-io-0.3.13
  (crate-source "futures-io" "0.3.13"
                "0ndshyr6vgnx0z421h2asd7jkhy2cwqj5ha1a8pw613pqmjjq76p"))

(define rust-futures-macro-0.3.13
  (crate-source "futures-macro" "0.3.13"
                "1dxzgz1sb0qin9nqsdrqc1aky52j3n9fnarcz2db77qkllb5hh7a"))

(define rust-futures-sink-0.3.13
  (crate-source "futures-sink" "0.3.13"
                "1cwbbd9dik0p54d53v4kd28lgj2a9bxgprp8ypabfhaqk2c4sxc5"))

(define rust-futures-task-0.3.13
  (crate-source "futures-task" "0.3.13"
                "106zr4rvl3w2h9iibljh4ba0g7pxq7hwyvvscq1fcpn127r9w67s"))

(define rust-futures-util-0.3.13
  (crate-source "futures-util" "0.3.13"
                "1lcn7nbar6gb47a2lvmjdarw5g6d78jf2hqsf3rddy7diamwf4hq"))

(define rust-humantime-1.3.0
  (crate-source "humantime" "1.3.0"
                "0krwgbf35pd46xvkqg14j070vircsndabahahlv3rwhflpy4q06z"))

(define rust-iovec-0.1.4
  (crate-source "iovec" "0.1.4"
                "0ph73qygwx8i0mblrf110cj59l00gkmsgrpzz1rm85syz5pymcxj"))

(define rust-kernel32-sys-0.2.2
  (crate-source "kernel32-sys" "0.2.2"
                "1389av0601a9yz8dvx5zha9vmkd6ik7ax0idpb032d28555n41vm"))

(define rust-libpulse-binding-2.23.0
  (crate-source "libpulse-binding" "2.23.0"
                "03khphfxifmmszh541hvnjhyksydhai4hv9b7pxpslh1d205yh5j"))

(define rust-libpulse-sys-1.18.0
  (crate-source "libpulse-sys" "1.18.0"
                "10msfr8f951v86ag0fl2bsm4a3siq2r7hz9bqhhg7i234s1yj5yg"))

(define rust-mio-0.6.23
  (crate-source "mio" "0.6.23"
                "1i2c1vl8lr45apkh8xbh9k56ihfsmqff5l7s2fya7whvp7sndzaa"))

(define rust-mio-uds-0.6.8
  (crate-source "mio-uds" "0.6.8"
                "1w36w09gd8as1mah80wdy0kgpshmphmljj68gij34hvdnag6kjxg"))

(define rust-miow-0.2.2
  (crate-source "miow" "0.2.2"
                "0kcl8rnv0bhiarcdakik670w8fnxzlxhi1ys7152sck68510in7b"))

(define rust-net2-0.2.37
  (crate-source "net2" "0.2.37"
                "1bk8jp0i12gvhrlaqbfq19ancja70r1rg3sywbhjl0385g8k05ir"))

(define rust-nix-0.15.0
  (crate-source "nix" "0.15.0"
                "0aa2l7wg9pzx24ks4p97gdy09a4hhs1sr9drxnm75v906d7hnbiv"))

(define rust-num-derive-0.3.3
  (crate-source "num-derive" "0.3.3"
                "0gbl94ckzqjdzy4j8b1p55mz01g6n1l9bckllqvaj0wfz7zm6sl7"))

(define rust-num-traits-0.2.14
  (crate-source "num-traits" "0.2.14"
                "144j176s2p76azy2ngk2vkdzgwdc0bc8c93jhki8c9fsbknb2r4s"))

(define rust-pin-project-lite-0.1.12
  (crate-source "pin-project-lite" "0.1.12"
                "0xx7f3wzc8ydvd1v2mmrxfypjchp52bphrirf08phbq8ba8n8yr5"))

(define rust-pin-project-lite-0.2.5
  (crate-source "pin-project-lite" "0.2.5"
                "12jlqmyw8sy3d5jc8ri4d3gg9z7wyl6rzjr2qz8kw0sb5r293x0c"))

(define rust-pin-utils-0.1.0
  (crate-source "pin-utils" "0.1.0"
                "117ir7vslsl2z1a7qzhws4pd01cg2d3338c47swjyvqv2n60v1wb"))

(define rust-pkg-config-0.3.19
  (crate-source "pkg-config" "0.3.19"
                "0k4860955riciibxr8bhnklp79jydp4xfylwdn5v9kj96hxlac9q"))

(define rust-proc-macro-error-1.0.4
  (crate-source "proc-macro-error" "1.0.4"
                "1373bhxaf0pagd8zkyd03kkx6bchzf6g0dkwrwzsnal9z47lj9fs"))

(define rust-proc-macro-error-attr-1.0.4
  (crate-source "proc-macro-error-attr" "1.0.4"
                "0sgq6m5jfmasmwwy8x4mjygx5l7kp8s4j60bv25ckv2j1qc41gm1"))

(define rust-proc-macro-hack-0.5.19
  (crate-source "proc-macro-hack" "0.5.19"
                "1rg0kzsj7lj00qj602d3h77spwfz48vixn1wbjp7a4yrq65w9w6v"))

(define rust-proc-macro-nested-0.1.7
  (crate-source "proc-macro-nested" "0.1.7"
                "11hh1jynh62f3m1ii0f9gf1l3y0fhkwpmr40lz3704v848n1p25w"))

(define rust-quick-error-1.2.3
  (crate-source "quick-error" "1.2.3"
                "1q6za3v78hsspisc197bg3g7rpc989qycy8ypr8ap8igv10ikl51"))

(define rust-regex-syntax-0.6.22
  (crate-source "regex-syntax" "0.6.22"
                "10b56ylil35jkb4nwqxm8hbyx3zq7fws0wpydjln165s8xql3sxm"))

(define rust-signal-hook-registry-1.3.0
  (crate-source "signal-hook-registry" "1.3.0"
                "19hirq0h33jjyh505s8hf9q5dq0ky80ygivkl3vshjv0y7zd1w8n"))

(define rust-slab-0.4.2
  (crate-source "slab" "0.4.2"
                "1y59xsa27jk84sxzswjk60xcjf8b4fm5960jwpznrrcmasyva4f1"))

(define rust-strsim-0.8.0
  (crate-source "strsim" "0.8.0"
                "0sjsm7hrvjdifz661pjxq5w4hf190hx53fra8dfvamacvff139cf"))

(define rust-structopt-0.3.21
  (crate-source "structopt" "0.3.21"
                "136j0lvjmpv5syi751vxg8vb30gfyv4k81x8d18kxrj6xvbsqxsj"))

(define rust-structopt-derive-0.4.14
  (crate-source "structopt-derive" "0.4.14"
                "143gjwvz3s86hwp070km83y25n8kqp5f01kb1dr19f4ilkywvaav"))

(define rust-termcolor-1.1.2
  (crate-source "termcolor" "1.1.2"
                "1x65i1ny4m6z1by62ra6wdcrd557p2ysm866x0pg60zby2cxizid"))

(define rust-textwrap-0.11.0
  (crate-source "textwrap" "0.11.0"
                "0q5hky03ik3y50s9sz25r438bc4nwhqc6dqwynv4wylc807n29nk"))

(define rust-tokio-0.2.25
  (crate-source "tokio" "0.2.25"
                "14l0rll6y1dyzh6qcd8rma2ch3wx0dxzxq8b54di744sjirs40v7"))

(define rust-tokio-macros-0.2.6
  (crate-source "tokio-macros" "0.2.6"
                "0ni60vnrf32r3wfhlahmnds1phx5d1xfbmyq9j0mz8kkzh5s0kg4"))

(define rust-vec-map-0.8.2
  (crate-source "vec_map" "0.8.2"
                "1481w9g1dw9rxp3l6snkdqihzyrd2f8vispzqmwjwsdyhw8xzggi"))

(define rust-void-1.0.2
  (crate-source "void" "1.0.2"
                "0zc8f0ksxvmhvgx4fdg0zyn6vdnbxd2xv9hfx4nhzg6kbs4f80ka"))

(define rust-winapi-0.2.8
  (crate-source "winapi" "0.2.8"
                "0yh816lh6lf56dpsgxy189c2ai1z3j8mw9si6izqb6wsjkbcjz8n"))

(define rust-winapi-build-0.1.1
  (crate-source "winapi-build" "0.1.1"
                "1g4rqsgjky0a7530qajn2bbfcrl2v0zb39idgdws9b1l7gp5wc9d"))

(define rust-ws2-32-sys-0.2.1
  (crate-source "ws2_32-sys" "0.2.1"
                "0ppscg5qfqaw0gzwv2a4nhn5bn01ff9iwn6ysqnzm4n8s3myz76m"))

(define rust-x11-2.18.2
  (crate-source "x11" "2.18.2"
                "0wz7l6dlbraa9zalh9i45v9wibvkir9m2m1sg0jnzcbcaj9d1v3p"))

(define rust-xcb-0.9.0
  (crate-source "xcb" "0.9.0"
                "19i2pm8alpn2f0m4jg8bsw6ckw8irj1wjh55h9pi2fcb2diny1b2"))

(define rust-xidlehook-core-0.3.0
  (crate-source "xidlehook-core" "0.3.0"
                "03p9fksj88z93lc3wqcvhz5c69zmznljzvg98dkljc51xp4bk0zh"))

;; Additional serde versions for xidlehook
(define rust-serde-1.0.123
  (crate-source "serde" "1.0.123"
                "1bk9733mgiv5sg8yb19y8mc85fb2aaqp1k02v10alavj688idmcj"))

(define rust-serde-derive-1.0.123
  (crate-source "serde_derive" "1.0.123"
                "0ccg4m7ww6mfs5vjdbdifri2kf1wyd4difjnqnraph2gssaw54ck"))

(define rust-serde-json-1.0.64
  (crate-source "serde_json" "1.0.64"
                "0y9gk3yikncrc0zajmwc0pidr7zfwafawb4gidf6mqyskzf9g7kr"))

(define rust-cfg-if-0.1.10
  (crate-source "cfg-if" "0.1.10"
                "08h80ihs74jcyp24cd75wwabygbbdgl05k6p5dmq8akbr78vv1a7"))

(define rust-itoa-0.4.7
  (crate-source "itoa" "0.4.7"
                "0di7fggbknwfjcw8cgzm1dnm3ik32l2m1f7nmyh8ipmh45h069fx"))

(define rust-ryu-1.0.5
  (crate-source "ryu" "1.0.5"
                "0vpqv1dj7fksa6hm3zpk5rbsjs0ifbfy7xwzsyyil0rx37a03lvi"))

;; Additional missing xidlehook dependencies
(define rust-aho-corasick-0.7.15
  (crate-source "aho-corasick" "0.7.15"
                "1rb8gzhljl8r87dpf2n5pnqnkl694casgns4ma0sqzd4zazzw13l"))

(define rust-bitflags-1.2.1
  (crate-source "bitflags" "1.2.1"
                "14qnd5nq8p2almk79m4m8ydqhd413yaxsyjp5xd19g3mikzf47fg"))

(define rust-heck-0.3.2
  (crate-source "heck" "0.3.2"
                "1b56s2c1ymdd0qmy31bw0ndhm31hcdamnhg3npp7ssrmc1ag9jw7"))

(define rust-lazy-static-1.4.0
  (crate-source "lazy_static" "1.4.0"
                "0in6ikhw8mgl33wjv6q6xfrb5b9jr16q8ygjy803fay4zcisvaz2"))

(define rust-libc-0.2.87
  (crate-source "libc" "0.2.87"
                "04r2lkffw4j29yk4pa75xr7dk8pb5w1bidbbjlzhly6n64fpap96"))

(define rust-log-0.4.14
  (crate-source "log" "0.4.14"
                "04175hv0v62shd82qydq58a48k3bjijmk54v38zgqlbxqkkbpfai"))

(define rust-memchr-2.3.4
  (crate-source "memchr" "2.3.4"
                "098m9clfs495illlw00hv2gg67mhm7jflld3msyclvi5m9xc9q8f"))

(define rust-once-cell-1.7.2
  (crate-source "once_cell" "1.7.2"
                "18qmpyfigg4ibdhjy5mwcjhzk9adwlgfaqv7nj430ivm86q0i2xg"))

(define rust-proc-macro2-1.0.24
  (crate-source "proc-macro2" "1.0.24"
                "0wcabxzrddcjmryndw8fpyxcq6rw63m701vx86xxf03y3bp081qy"))

(define rust-quote-1.0.9
  (crate-source "quote" "1.0.9"
                "19rjmfqzk26rxbgxy5j2ckqc2v12sw2xw8l4gi8bzpn2bmsbkl63"))

(define rust-regex-1.4.3
  (crate-source "regex" "1.4.3"
                "12llbg82js69mdl50lav4yn1iqlx71ckb18dww467q99w4wi49fr"))

(define rust-syn-1.0.60
  (crate-source "syn" "1.0.60"
                "1080gw6mlja7yl26crya3k403wjdp7v3wx9mxcmpcnlar9z5j067"))

(define rust-thread-local-1.1.3
  (crate-source "thread_local" "1.1.3"
                "1gccp3grndpi6dyhzylz4hkqnkzc1xyri98n0xwwhnn90i7d4640"))

(define rust-unicode-segmentation-1.7.1
  (crate-source "unicode-segmentation" "1.7.1"
                "15n736z0pbj30pj44jb9s9rjavzrmx8v8pzdgsl5yfmfwrxjw3dv"))

(define rust-unicode-width-0.1.8
  (crate-source "unicode-width" "0.1.8"
                "1qxizyi6xbcqyi4z79p523ywvmgsfcgfqb3zv3c8i6x1jcc5jdwk"))

(define rust-unicode-xid-0.2.1
  (crate-source "unicode-xid" "0.2.1"
                "0r6mknipyy9vpz8mwmxvkx65ff2ha1n2pxqjj6f46lcn8yrhpzpp"))

(define rust-version-check-0.9.2
  (crate-source "version_check" "0.9.2"
                "1vbaqdf802qinsq8q20w8w0qn2pv0rkq5p73ijcblrwxcvjp5adm"))

(define rust-winapi-util-0.1.5
  (crate-source "winapi-util" "0.1.5"
                "0y71bp7f6d536czj40dhqk0d55wfbbwqfp2ymqf1an5ibgl6rv3h"))

;; Shadowplay dependencies
(define rust-ansi-term-0.12.1
  (crate-source "ansi_term" "0.12.1"
                "1ljmkbilxgmhavxvxqa7qvm6f3fjggi7q2l3a72q9x0cxjvrnanm"))

(define rust-anyhow-1.0.99
  (crate-source "anyhow" "1.0.99"
                "001icqvkfl28rxxmk99rm4gvdzxqngj5v50yg2bh3dzcvqfllrxh"))

(define rust-arrayvec-0.5.2
  (crate-source "arrayvec" "0.5.2"
                "12q6hn01x5435bprwlb7w9m7817dyfq55yrl4psygr78bp32zdi3"))

(define rust-autocfg-1.5.0
  (crate-source "autocfg" "1.5.0"
                "1s77f98id9l4af4alklmzq46f21c980v13z2r1pcxx6bqgw0d1n0"))

(define rust-bytecount-0.6.9
  (crate-source "bytecount" "0.6.9"
                "0pinq0n8zza8qr2lyc3yf17k963129kdbf0bwnmvdk1bpvh14n0p"))

(define rust-clap-2.34.0
  (crate-source "clap" "2.34.0"
                "071q5d8jfwbazi6zhik9xwpacx5i6kb2vkzy060vhf0c3120aqd0"))

(define rust-env-logger-0.9.3
  (crate-source "env_logger" "0.9.3"
                "1rq0kqpa8my6i1qcyhfqrn1g9xr5fbkwwbd42nqvlzn9qibncbm1"))

(define rust-hashbrown-0.12.3
  (crate-source "hashbrown" "0.12.3"
                "1268ka4750pyg2pbgsr43f0289l5zah4arir2k4igx5a8c6fg7la"))

(define rust-heck-0.3.3
  (crate-source "heck" "0.3.3"
                "0b0kkr790p66lvzn9nsmfjvydrbmh9z5gb664jchwgw64vxiwqkd"))

(define rust-hermit-abi-0.1.19
  (crate-source "hermit-abi" "0.1.19"
                "0cxcm8093nf5fyn114w8vxbrbcyvv91d4015rdnlgfll7cs6gd32"))

(define rust-humantime-2.2.0
  (crate-source "humantime" "2.2.0"
                "17rz8jhh1mcv4b03wnknhv1shwq2v9vhkhlfg884pprsig62l4cv"))

(define rust-indexmap-1.9.3
  (crate-source "indexmap" "1.9.3"
                "16dxmy7yvk51wvnih3a3im6fp5lmx0wx76i03n06wyak6cwhw1xx"))

(define rust-itoa-1.0.15
  (crate-source "itoa" "1.0.15"
                "0b4fj9kz54dr3wam0vprjwgygvycyw8r0qwg7vp19ly8b2w16psa"))

(define rust-libc-0.2.175
  (crate-source "libc" "0.2.175"
                "0hw5sb3gjr0ivah7s3fmavlpvspjpd4mr009abmam2sr7r4sx0ka"))

(define rust-linked-hash-map-0.5.6
  (crate-source "linked-hash-map" "0.5.6"
                "03vpgw7x507g524nx5i1jf5dl8k3kv0fzg8v3ip6qqwbpkqww5q7"))

(define rust-located-yaml-0.2.1
  (crate-source "located_yaml" "0.2.1"
                "0xnx5al5v7d9syspj0irm22alwc3a9adikqxpbyyf6vsz3k8xilv"))

(define rust-log-0.4.27
  (crate-source "log" "0.4.27"
                "150x589dqil307rv0rwj0jsgz5bjbwvl83gyl61jf873a7rjvp0k"))

(define rust-memchr-2.7.5
  (crate-source "memchr" "2.7.5"
                "1h2bh2jajkizz04fh047lpid5wgw2cr9igpkdhl3ibzscpd858ij"))

(define rust-minimal-lexical-0.2.1
  (crate-source "minimal-lexical" "0.2.1"
                "16ppc5g84aijpri4jzv14rvcnslvlpphbszc7zzp6vfkddf4qdb8"))

(define rust-nom-7.1.3
  (crate-source "nom" "7.1.3"
                "0jha9901wxam390jcf5pfa0qqfrgh8li787jx2ip0yk5b8y9hwyj"))

(define rust-nom-locate-4.2.0
  (crate-source "nom_locate" "4.2.0"
                "1wx87c2pm84h63rb4rsjrqzgx574x1zy93av1jk3swdhag086g0y"))

(define rust-pretty-0.11.3
  (crate-source "pretty" "0.11.3"
                "0ikq0dfcgj99m8z50zpkfz140sqpmiji4imp9l93nzd87hgamww3"))

(define rust-proc-macro-error-1.0.4
  (crate-source "proc-macro-error" "1.0.4"
                "1373bhxaf0pagd8zkyd03kkx6bchzf6g0dkwrwzsnal9z47lj9fs"))

(define rust-proc-macro-error-attr-1.0.4
  (crate-source "proc-macro-error-attr" "1.0.4"
                "0sgq6m5jfmasmwwy8x4mjygx5l7kp8s4j60bv25ckv2j1qc41gm1"))

(define rust-proc-macro2-1.0.101
  (crate-source "proc-macro2" "1.0.101"
                "1pijhychkpl7rcyf1h7mfk6gjfii1ywf5n0snmnqs5g4hvyl7bl9"))

(define rust-quote-1.0.40
  (crate-source "quote" "1.0.40"
                "1394cxjg6nwld82pzp2d4fp6pmaz32gai1zh9z5hvh0dawww118q"))

(define rust-regex-1.11.2
  (crate-source "regex" "1.11.2"
                "04k9rzxd11hcahpyihlswy6f1zqw7lspirv4imm4h0lcdl8gvmr3"))

(define rust-regex-automata-0.4.10
  (crate-source "regex-automata" "0.4.10"
                "1mllcfmgjcl6d52d5k09lwwq9wj5mwxccix4bhmw5spy1gx5i53b"))

(define rust-regex-syntax-0.8.6
  (crate-source "regex-syntax" "0.8.6"
                "00chjpglclfskmc919fj5aq308ffbrmcn7kzbkz92k231xdsmx6a"))

(define rust-ryu-1.0.20
  (crate-source "ryu" "1.0.20"
                "07s855l8sb333h6bpn24pka5sp7hjk2w667xy6a0khkf6sqv5lr8"))

(define rust-serde-1.0.219
  (crate-source "serde" "1.0.219"
                "1dl6nyxnsi82a197sd752128a4avm6mxnscywas1jq30srp2q3jz"))

(define rust-serde-derive-1.0.219
  (crate-source "serde_derive" "1.0.219"
                "001azhjmj7ya52pmfiw4ppxm16nd44y15j2pf5gkcwrcgz7pc0jv"))

(define rust-serde-json-1.0.143
  (crate-source "serde_json" "1.0.143"
                "0njabwzldvj13ykrf1aaf4gh5cgl25kf9hzbpafbv3qh3ppsn0fl"))

(define rust-serde-regex-1.1.0
  (crate-source "serde_regex" "1.1.0"
                "1pxsnxb8c198szghk1hvzvhva36w2q5zs70hqkmdf5d89qd6y4x8"))

(define rust-serde-yaml-0.8.26
  (crate-source "serde_yaml" "0.8.26"
                "06y7gxy312mink8nsnmci9cw0ykpgsdcxmayg0snmdbnnwrp92jp"))

(define rust-structopt-0.3.26
  (crate-source "structopt" "0.3.26"
                "043sg3qxllann6q9i71d05qp3q13scmcvhxhd950ka2v8ij5qsqc"))

(define rust-structopt-derive-0.4.18
  (crate-source "structopt-derive" "0.4.18"
                "1q5gcigmvw0cinjxzpyrkflliq5r1ivljmrvfrl3phcwgwraxdfw"))

(define rust-syn-2.0.106
  (crate-source "syn" "2.0.106"
                "19mddxp1ia00hfdzimygqmr1jqdvyl86k48427bkci4d08wc9rzd"))

(define rust-termcolor-1.4.1
  (crate-source "termcolor" "1.4.1"
                "0mappjh3fj3p2nmrg4y7qv94rchwi9mzmgmfflr8p2awdj7lyy86"))

(define rust-typed-arena-2.0.2
  (crate-source "typed-arena" "2.0.2"
                "0shj0jpmglhgw2f1i4b33ycdzwd1z205pbs1rd5wx7ks2qhaxxka"))

(define rust-unicode-ident-1.0.18
  (crate-source "unicode-ident" "1.0.18"
                "04k5r6sijkafzljykdq26mhjpmhdx4jwzvn1lh90g9ax9903jpss"))

(define rust-unicode-width-0.1.14
  (crate-source "unicode-width" "0.1.14"
                "1bzn2zv0gp8xxbxbhifw778a7fc93pa6a1kj24jgg9msj07f7mkx"))

(define rust-winapi-util-0.1.10
  (crate-source "winapi-util" "0.1.10"
                "08hb8rj3aq9lcrfmliqs4l7v9zh6srbcn0376yn0pndkf5qvyy09"))

(define rust-windows-link-0.1.3
  (crate-source "windows-link" "0.1.3"
                "12kr1p46dbhpijr4zbwr2spfgq8i8c5x55mvvfmyl96m01cx4sjy"))

(define rust-windows-sys-0.60.2
  (crate-source "windows-sys" "0.60.2"
                "1jrbc615ihqnhjhxplr2kw7rasrskv9wj3lr80hgfd42sbj01xgj"))

(define rust-windows-targets-0.53.3
  (crate-source "windows-targets" "0.53.3"
                "14fwwm136dhs3i1impqrrip7nvkra3bdxa4nqkblj604qhqn1znm"))

(define rust-windows-aarch64-gnullvm-0.53.0
  (crate-source "windows_aarch64_gnullvm" "0.53.0"
                "0r77pbpbcf8bq4yfwpz2hpq3vns8m0yacpvs2i5cn6fx1pwxbf46"))

(define rust-windows-aarch64-msvc-0.53.0
  (crate-source "windows_aarch64_msvc" "0.53.0"
                "0v766yqw51pzxxwp203yqy39ijgjamp54hhdbsyqq6x1c8gilrf7"))

(define rust-windows-i686-gnu-0.53.0
  (crate-source "windows_i686_gnu" "0.53.0"
                "1hvjc8nv95sx5vdd79fivn8bpm7i517dqyf4yvsqgwrmkmjngp61"))

(define rust-windows-i686-gnullvm-0.53.0
  (crate-source "windows_i686_gnullvm" "0.53.0"
                "04df1in2k91qyf1wzizvh560bvyzq20yf68k8xa66vdzxnywrrlw"))

(define rust-windows-i686-msvc-0.53.0
  (crate-source "windows_i686_msvc" "0.53.0"
                "0pcvb25fkvqnp91z25qr5x61wyya12lx8p7nsa137cbb82ayw7sq"))

(define rust-windows-x86-64-gnu-0.53.0
  (crate-source "windows_x86_64_gnu" "0.53.0"
                "1flh84xkssn1n6m1riddipydcksp2pdl45vdf70jygx3ksnbam9f"))

(define rust-windows-x86-64-gnullvm-0.53.0
  (crate-source "windows_x86_64_gnullvm" "0.53.0"
                "0mvc8119xpbi3q2m6mrjcdzl6afx4wffacp13v76g4jrs1fh6vha"))

(define rust-windows-x86-64-msvc-0.53.0
  (crate-source "windows_x86_64_msvc" "0.53.0"
                "11h4i28hq0zlnjcaqi2xdxr7ibnpa8djfggch9rki1zzb8qi8517"))

(define rust-yaml-rust-0.4.5
  (crate-source "yaml-rust" "0.4.5"
                "118wbqrr4n6wgk5rjjnlrdlahawlxc1bdsx146mwk8f79in97han"))

;; Define cargo inputs mapping
(define-cargo-inputs lookup-cargo-inputs
  (i3im =>
        (list
         ;; Direct dependencies
         rust-anyhow-1.0.95
         rust-clap-4.5.23
         rust-i3ipc-jl-0.11.2
         rust-regex-1.11.1
         rust-serde-1.0.216
         rust-serde-json-1.0.134
         rust-serde-regex-1.1.0
         rust-serde-yaml-0.9.34+deprecated
         rust-shellexpand-3.1.0
         rust-slog-2.7.0
         rust-slog-envlogger-2.2.0
         rust-slog-scope-4.4.0
         rust-slog-stdlog-4.1.1
         rust-slog-syslog-jl-0.13.1
         rust-slog-term-2.9.1
         rust-structdoc-0.1.4
         ;; Transitive dependencies
         rust-aho-corasick-1.1.3
         rust-anstream-0.6.18
         rust-anstyle-1.0.10
         rust-anstyle-parse-0.2.6
         rust-anstyle-query-1.1.2
         rust-anstyle-wincon-3.0.6
         rust-arc-swap-1.7.1
         rust-bitflags-1.3.2
         rust-bitflags-2.6.0
         rust-byteorder-1.5.0
         rust-cfg-if-1.0.0
         rust-clap-builder-4.5.23
         rust-clap-derive-4.5.18
         rust-clap-lex-0.7.4
         rust-colorchoice-1.0.3
         rust-crossbeam-channel-0.5.14
         rust-crossbeam-utils-0.8.21
         rust-deranged-0.3.11
         rust-dirs-5.0.1
         rust-dirs-next-2.0.0
         rust-dirs-sys-0.4.1
         rust-dirs-sys-next-0.1.2
         rust-either-1.13.0
         rust-equivalent-1.0.1
         rust-error-chain-0.12.4
         rust-getrandom-0.2.15
         rust-hashbrown-0.15.2
         rust-heck-0.3.3
         rust-heck-0.5.0
         rust-hermit-abi-0.4.0
         rust-indexmap-2.7.0
         rust-is-terminal-0.4.13
         rust-is-terminal-polyfill-1.70.1
         rust-itertools-0.8.2
         rust-itoa-1.0.14
         rust-lazy-static-1.5.0
         rust-libc-0.2.169
         rust-libredox-0.1.3
         rust-log-0.4.22
         rust-memchr-2.7.4
         rust-num-conv-0.1.0
         rust-once-cell-1.20.2
         rust-option-ext-0.2.0
         rust-powerfmt-0.2.0
         rust-proc-macro2-1.0.92
         rust-quote-1.0.37
         rust-redox-users-0.4.6
         rust-regex-automata-0.4.9
         rust-regex-syntax-0.8.5
         rust-rustversion-1.0.18
         rust-ryu-1.0.18
         rust-serde-derive-1.0.216
         rust-slog-async-2.8.0
         rust-strsim-0.11.1
         rust-structdoc-derive-0.1.4
         rust-syn-1.0.109
         rust-syn-2.0.91
         rust-syslog-5.0.0
         rust-take-mut-0.2.2
         rust-term-0.7.0
         rust-thiserror-1.0.69
         rust-thiserror-impl-1.0.69
         rust-thread-local-1.1.8
         rust-time-0.1.45
         rust-time-0.3.37
         rust-time-core-0.1.2
         rust-time-macros-0.2.19
         rust-unicode-ident-1.0.14
         rust-unicode-segmentation-1.12.0
         rust-unindent-0.1.11
         rust-unsafe-libyaml-0.2.11
         rust-utf8parse-0.2.2
         rust-version-check-0.9.5
         rust-wasi-0.10.0+wasi-snapshot-preview1
         rust-wasi-0.11.0+wasi-snapshot-preview1
         rust-winapi-0.3.9
         rust-winapi-i686-pc-windows-gnu-0.4.0
         rust-winapi-x86-64-pc-windows-gnu-0.4.0
         rust-windows-sys-0.48.0
         rust-windows-sys-0.52.0
         rust-windows-sys-0.59.0
         rust-windows-targets-0.48.5
         rust-windows-targets-0.52.6
         rust-windows-aarch64-gnullvm-0.48.5
         rust-windows-aarch64-gnullvm-0.52.6
         rust-windows-aarch64-msvc-0.48.5
         rust-windows-aarch64-msvc-0.52.6
         rust-windows-i686-gnu-0.48.5
         rust-windows-i686-gnu-0.52.6
         rust-windows-i686-gnullvm-0.52.6
         rust-windows-i686-msvc-0.48.5
         rust-windows-i686-msvc-0.52.6
         rust-windows-x86-64-gnu-0.48.5
         rust-windows-x86-64-gnu-0.52.6
         rust-windows-x86-64-gnullvm-0.48.5
         rust-windows-x86-64-gnullvm-0.52.6
         rust-windows-x86-64-msvc-0.48.5
         rust-windows-x86-64-msvc-0.52.6))
  (xidlehook =>
             (list
              ;; Direct dependencies  
              rust-clap-2.33.3
              rust-env-logger-0.7.1
              rust-libpulse-binding-2.23.0
              rust-nix-0.15.0
              rust-x11-2.18.2
              rust-xcb-0.9.0
              rust-xidlehook-core-0.3.0
              ;; Transitive dependencies
              rust-aho-corasick-0.7.15
              rust-ansi-term-0.11.0
              rust-atty-0.2.14
              rust-autocfg-1.0.1
              rust-bitflags-1.2.1
              rust-bitflags-1.3.2
              rust-bytes-0.5.6
              rust-cc-1.0.67
              rust-cfg-if-0.1.10
              rust-cfg-if-1.0.0
              rust-env-logger-0.7.1
              rust-fnv-1.0.7
              rust-fuchsia-zircon-0.3.3
              rust-fuchsia-zircon-sys-0.3.3
              rust-futures-0.3.13
              rust-futures-channel-0.3.13
              rust-futures-core-0.3.13
              rust-futures-executor-0.3.13
              rust-futures-io-0.3.13
              rust-futures-macro-0.3.13
              rust-futures-sink-0.3.13
              rust-futures-task-0.3.13
              rust-futures-util-0.3.13
              rust-heck-0.3.2
              rust-heck-0.3.3
              rust-hermit-abi-0.1.18
              rust-humantime-1.3.0
              rust-iovec-0.1.4
              rust-itoa-0.4.7
              rust-kernel32-sys-0.2.2
              rust-lazy-static-1.4.0
              rust-lazy-static-1.5.0
              rust-libc-0.2.87
              rust-libc-0.2.169
              rust-libpulse-sys-1.18.0
              rust-log-0.4.14
              rust-log-0.4.22
              rust-memchr-2.3.4
              rust-memchr-2.7.4
              rust-mio-0.6.23
              rust-mio-uds-0.6.8
              rust-miow-0.2.2
              rust-net2-0.2.37
              rust-num-derive-0.3.3
              rust-num-traits-0.2.14
              rust-once-cell-1.7.2
              rust-once-cell-1.20.2
              rust-pin-project-lite-0.1.12
              rust-pin-project-lite-0.2.5
              rust-pin-utils-0.1.0
              rust-pkg-config-0.3.19
              rust-proc-macro-error-1.0.4
              rust-proc-macro-error-attr-1.0.4
              rust-proc-macro-hack-0.5.19
              rust-proc-macro-nested-0.1.7
              rust-proc-macro2-1.0.24
              rust-proc-macro2-1.0.92
              rust-quick-error-1.2.3
              rust-quote-1.0.9
              rust-quote-1.0.37
              rust-regex-1.4.3
              rust-regex-1.11.1
              rust-regex-automata-0.4.9
              rust-regex-syntax-0.6.22
              rust-regex-syntax-0.8.5
              rust-ryu-1.0.5
              rust-serde-1.0.123
              rust-serde-derive-1.0.123
              rust-serde-json-1.0.64
              rust-slab-0.4.2
              rust-signal-hook-registry-1.3.0
              rust-strsim-0.8.0
              rust-structopt-0.3.21
              rust-structopt-derive-0.4.14
              rust-syn-1.0.60
              rust-syn-1.0.109
              rust-termcolor-1.1.2
              rust-textwrap-0.11.0
              rust-thread-local-1.1.3
              rust-thread-local-1.1.8
              rust-tokio-0.2.25
              rust-tokio-macros-0.2.6
              rust-unicode-segmentation-1.7.1
              rust-unicode-segmentation-1.12.0
              rust-unicode-width-0.1.8
              rust-unicode-xid-0.2.1
              rust-vec-map-0.8.2
              rust-version-check-0.9.2
              rust-version-check-0.9.5
              rust-void-1.0.2
              rust-winapi-0.2.8
              rust-winapi-0.3.9
              rust-winapi-build-0.1.1
              rust-winapi-i686-pc-windows-gnu-0.4.0
              rust-winapi-util-0.1.5
              rust-winapi-x86-64-pc-windows-gnu-0.4.0
              rust-ws2-32-sys-0.2.1))
  (shadowplay =>
              (list
               ;; Direct dependencies
               rust-ansi-term-0.12.1
               rust-anyhow-1.0.99
               rust-clap-2.34.0
               rust-env-logger-0.9.3
               rust-located-yaml-0.2.1
               rust-nom-7.1.3
               rust-nom-locate-4.2.0
               rust-pretty-0.11.3
               rust-serde-1.0.219
               rust-serde-json-1.0.143
               rust-serde-regex-1.1.0
               rust-serde-yaml-0.8.26
               rust-structopt-0.3.26
               rust-yaml-rust-0.4.5
               ;; Transitive dependencies
               rust-aho-corasick-1.1.3
               rust-arrayvec-0.5.2
               rust-atty-0.2.14
               rust-autocfg-1.5.0
               rust-bitflags-1.3.2
               rust-bytecount-0.6.9
               rust-hashbrown-0.12.3
               rust-heck-0.3.3
               rust-hermit-abi-0.1.19
               rust-humantime-2.2.0
               rust-indexmap-1.9.3
               rust-itoa-1.0.15
               rust-lazy-static-1.5.0
               rust-libc-0.2.175
               rust-linked-hash-map-0.5.6
               rust-log-0.4.27
               rust-memchr-2.7.5
               rust-minimal-lexical-0.2.1
               rust-proc-macro-error-1.0.4
               rust-proc-macro-error-attr-1.0.4
               rust-proc-macro2-1.0.101
               rust-quote-1.0.40
               rust-regex-1.11.2
               rust-regex-automata-0.4.10
               rust-regex-syntax-0.8.6
               rust-ryu-1.0.20
               rust-serde-derive-1.0.219
               rust-strsim-0.8.0
               rust-structopt-derive-0.4.18
               rust-syn-1.0.109
               rust-syn-2.0.106
               rust-termcolor-1.4.1
               rust-textwrap-0.11.0
               rust-typed-arena-2.0.2
               rust-unicode-ident-1.0.18
               rust-unicode-segmentation-1.12.0
               rust-unicode-width-0.1.14
               rust-unicode-xid-0.2.1
               rust-vec-map-0.8.2
               rust-version-check-0.9.5
               rust-winapi-0.3.9
               rust-winapi-i686-pc-windows-gnu-0.4.0
               rust-winapi-x86-64-pc-windows-gnu-0.4.0
               rust-winapi-util-0.1.10
               rust-windows-link-0.1.3
               rust-windows-sys-0.60.2
               rust-windows-targets-0.53.3
               rust-windows-aarch64-gnullvm-0.53.0
               rust-windows-aarch64-msvc-0.53.0
               rust-windows-i686-gnu-0.53.0
               rust-windows-i686-gnullvm-0.53.0
               rust-windows-i686-msvc-0.53.0
               rust-windows-x86-64-gnu-0.53.0
               rust-windows-x86-64-gnullvm-0.53.0
               rust-windows-x86-64-msvc-0.53.0)))