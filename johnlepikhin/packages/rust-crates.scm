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
  #:use-module (guix build-system cargo)
  #:export (lookup-cargo-inputs))

;; Crate sources for i3im dependencies
(define rust-addr2line-0.24.2
  (crate-source "addr2line" "0.24.2"
                "1hd1i57zxgz08j6h5qrhsnm2fi0bcqvsh389fw400xm3arz2ggnz"))

(define rust-adler2-2.0.0
  (crate-source "adler2" "2.0.0"
                "09r6drylvgy8vv8k20lnbvwq8gp09h7smfn6h1rxsy15pgh629si"))

(define rust-aho-corasick-1.1.3
  (crate-source "aho-corasick" "1.1.3"
                "05mrpkvdgp5d20y2p989f187ry9diliijgwrs254fs9s1m1x6q4f"))

(define rust-aho-corasick-0.7.20
  (crate-source "aho-corasick" "0.7.20"
                "1b3if3nav4qzgjz9bf75b2cv2h2yisrqfs0np70i38kgz4cn94yc"))

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

(define rust-autocfg-1.4.0
  (crate-source "autocfg" "1.4.0"
                "09lz3by90d2hphbq56znag9v87gfpd9gb8nr82hll8z6x2nhprdc"))

(define rust-arc-swap-1.7.1
  (crate-source "arc-swap" "1.7.1"
                "0mrl9a9r9p9bln74q6aszvf22q1ijiw089jkrmabfqkbj31zixv9"))

(define rust-backtrace-0.3.74
  (crate-source "backtrace" "0.3.74"
                "06pfif7nwx66qf2zaanc2fcq7m64i91ki9imw9xd3bnz5hrwp0ld"))

(define rust-bitflags-1.3.2
  (crate-source "bitflags" "1.3.2"
                "12ki6w8gn1ldq7yz9y680llwk5gmrhrzszaa17g1sbrw2r2qvwxy"))

(define rust-bitflags-2.6.0
  (crate-source "bitflags" "2.6.0"
                "1pkidwzn3hnxlsl8zizh0bncgbjnw7c41cx7bby26ncbzmiznj5h"))

(define rust-block-buffer-0.10.4
  (crate-source "block-buffer" "0.10.4"
                "0w9sa2ypmrsqqvc20nhwr75wbb5cjr4kkyhpjm1z1lv2kdicfy1h"))

(define rust-byteorder-1.5.0
  (crate-source "byteorder" "1.5.0"
                "0jzncxyf404mwqdbspihyzpkndfgda450l0893pz5xj685cg5l0z"))

(define rust-cc-1.2.6
  (crate-source "cc" "1.2.6"
                "0cx32v9pcslavf8y10sb3y883v7377mw48q3dpw5b1cgidibnvcd"))

(define rust-cfg-aliases-0.2.1
  (crate-source "cfg_aliases" "0.2.1"
                "092pxdc1dbgjb6qvh83gk56rkic2n2ybm4yvy76cgynmzi3zwfk1"))

(define rust-cfg-if-1.0.0
  (crate-source "cfg-if" "1.0.0"
                "1za0vb97n4brpzpv8lsbnzmq5r8f2b0cpqqr0sy8h5bn751xxwds"))

(define rust-clap-4.5.23
  (crate-source "clap" "4.5.23"
                "110cf0i9fmkfqzqhi1h8za9y0vnr5rwhy3wmv1p0rcgp5vnffd9i"))

(define rust-clap-4.5.27
  (crate-source "clap" "4.5.27"
                "15j720q1z953h1qxm2q5nwkmyhhl2vb45v017rqlhjrbk12h36vn"))

(define rust-clap-builder-4.5.23
  (crate-source "clap_builder" "4.5.23"
                "0f28rgc09kdgfq1hgg1bb1ydaw243w6dwyw74syz439k6b32yn1h"))

(define rust-clap-builder-4.5.27
  (crate-source "clap_builder" "4.5.27"
                "1mys7v60lys8zkwpk49wif9qnja9zamm4dnrsbj40wdmni78h9hv"))

(define rust-clap-complete-4.5.43
  (crate-source "clap_complete" "4.5.43"
                "0q8q8qsk3f6c1g5wk05i5836qp3rbdjh45j91ykdrin98lsh2lh9"))

(define rust-clap-derive-4.5.18
  (crate-source "clap_derive" "4.5.18"
                "1ardb26bvcpg72q9myr7yir3a8c83gx7vxk1cccabsd9n73s1ija"))

(define rust-clap-derive-4.5.24
  (crate-source "clap_derive" "4.5.24"
                "131ih3dm76srkbpfx7zfspp9b556zgzj31wqhl0ji2b39lcmbdsl"))

(define rust-clap-lex-0.7.4
  (crate-source "clap_lex" "0.7.4"
                "19nwfls5db269js5n822vkc8dw0wjq2h1wf0hgr06ld2g52d2spl"))

(define rust-cmake-0.1.52
  (crate-source "cmake" "0.1.52"
                "03k2haq0zqqpwrz8p9kq2qdkyk44a69lp9k3gxmmn3kycwiw50n6"))

(define rust-colorchoice-1.0.3
  (crate-source "colorchoice" "1.0.3"
                "1439m3r3jy3xqck8aa13q658visn71ki76qa93cy55wkmalwlqsv"))

(define rust-console-0.15.10
  (crate-source "console" "0.15.10"
                "06q4ag46machxp5w381x1v9l2g7d801q6sawvxcpidarh36nwg7a"))

(define rust-cpufeatures-0.2.16
  (crate-source "cpufeatures" "0.2.16"
                "1hy466fkhxjbb16i7na95wz8yr14d0kd578pwzj5lbkz14jh5f0n"))

(define rust-crossbeam-channel-0.5.14
  (crate-source "crossbeam-channel" "0.5.14"
                "0wa41qybq5w8s70anb472myh4fid4aw6v65vws6wn528w9l6vfh6"))

(define rust-crossbeam-utils-0.8.21
  (crate-source "crossbeam-utils" "0.8.21"
                "0a3aa2bmc8q35fb67432w16wvi54sfmb69rk9h5bhd18vw0c99fh"))

(define rust-crypto-common-0.1.6
  (crate-source "crypto-common" "0.1.6"
                "1cvby95a6xg7kxdz5ln3rl9xh66nz66w46mm3g56ri1z5x815yqv"))

(define rust-ctrlc-3.4.5
  (crate-source "ctrlc" "3.4.5"
                "1lqdhyl8csq8l2011g4w3wjps84w2cmwfn7jhx79ngrgm45apvlh"))

(define rust-darling-0.20.10
  (crate-source "darling" "0.20.10"
                "1299h2z88qn71mizhh05j26yr3ik0wnqmw11ijds89l8i9nbhqvg"))

(define rust-darling-core-0.20.10
  (crate-source "darling_core" "0.20.10"
                "1rgr9nci61ahnim93yh3xy6fkfayh7sk4447hahawah3m1hkh4wm"))

(define rust-darling-macro-0.20.10
  (crate-source "darling_macro" "0.20.10"
                "01kq3ibbn47czijj39h3vxyw0c2ksd0jvc097smcrk7n2jjs4dnk"))

(define rust-deranged-0.3.11
  (crate-source "deranged" "0.3.11"
                "1d1ibqqnr5qdrpw8rclwrf1myn3wf0dygl04idf4j2s49ah6yaxl"))

(define rust-derive-builder-0.20.2
  (crate-source "derive_builder" "0.20.2"
                "0is9z7v3kznziqsxa5jqji3ja6ay9wzravppzhcaczwbx84znzah"))

(define rust-derive-builder-core-0.20.2
  (crate-source "derive_builder_core" "0.20.2"
                "1s640r6q46c2iiz25sgvxw3lk6b6v5y8hwylng7kas2d09xwynrd"))

(define rust-derive-builder-macro-0.20.2
  (crate-source "derive_builder_macro" "0.20.2"
                "0g1zznpqrmvjlp2w7p0jzsjvpmw5rvdag0rfyypjhnadpzib0qxb"))

(define rust-dialoguer-0.11.0
  (crate-source "dialoguer" "0.11.0"
                "1pl0744wwr97kp8qnaybzgrfwk66qakzq0i1qrxl03vpbn0cx2v5"))

(define rust-digest-0.10.7
  (crate-source "digest" "0.10.7"
                "14p2n6ih29x81akj097lvz7wi9b6b9hvls0lwrv7b6xwyy0s5ncy"))

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

(define rust-displaydoc-0.2.5
  (crate-source "displaydoc" "0.2.5"
                "1q0alair462j21iiqwrr21iabkfnb13d6x5w95lkdg21q2xrqdlp"))

(define rust-either-1.13.0
  (crate-source "either" "1.13.0"
                "1w2c1mybrd7vljyxk77y9f4w9dyjrmp3yp82mk7bcm8848fazcb0"))

(define rust-encode-unicode-1.0.0
  (crate-source "encode_unicode" "1.0.0"
                "1h5j7j7byi289by63s3w4a8b3g6l5ccdrws7a67nn07vdxj77ail"))

(define rust-equivalent-1.0.1
  (crate-source "equivalent" "1.0.1"
                "1malmx5f4lkfvqasz319lq6gb3ddg19yzf9s8cykfsgzdmyq0hsl"))

(define rust-errno-0.3.10
  (crate-source "errno" "0.3.10"
                "0pgblicz1kjz9wa9m0sghkhh2zw1fhq1mxzj7ndjm746kg5m5n1k"))

(define rust-error-chain-0.12.4
  (crate-source "error-chain" "0.12.4"
                "1z6y5isg0il93jp287sv7pn10i4wrkik2cpyk376wl61rawhcbrd"))

(define rust-fastrand-2.3.0
  (crate-source "fastrand" "2.3.0"
                "1ghiahsw1jd68df895cy5h3gzwk30hndidn3b682zmshpgmrx41p"))

(define rust-form-urlencoded-1.2.1
  (crate-source "form_urlencoded" "1.2.1"
                "0milh8x7nl4f450s3ddhg57a3flcv6yq8hlkyk6fyr3mcb128dp1"))

(define rust-fuzzy-matcher-0.3.7
  (crate-source "fuzzy-matcher" "0.3.7"
                "153csv8rsk2vxagb68kpmiknvdd3bzqj03x805khckck28rllqal"))

(define rust-generic-array-0.14.7
  (crate-source "generic-array" "0.14.7"
                "16lyyrzrljfq424c3n8kfwkqihlimmsg5nhshbbp48np3yjrqr45"))

(define rust-getrandom-0.2.15
  (crate-source "getrandom" "0.2.15"
                "1mzlnrb3dgyd1fb84gvw10pyr8wdqdl4ry4sr64i1s8an66pqmn4"))

(define rust-gimli-0.31.1
  (crate-source "gimli" "0.31.1"
                "0gvqc0ramx8szv76jhfd4dms0zyamvlg4whhiz11j34hh3dqxqh7"))

(define rust-git2-0.20.0
  (crate-source "git2" "0.20.0"
                "1zwav0r76njd9chqxh7wj4r4zfn08nzsisrg05liyd6cjf4piniz"))

(define rust-handlebars-6.3.0
  (crate-source "handlebars" "6.3.0"
                "1n8kp12ci4n6qydrbf5vkx3g3vmjcgamlckh0an6irn1jm5j4srx"))

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

(define rust-icu-collections-1.5.0
  (crate-source "icu_collections" "1.5.0"
                "09j5kskirl59mvqc8kabhy7005yyy7dp88jw9f6f3gkf419a8byv"))

(define rust-icu-locid-1.5.0
  (crate-source "icu_locid" "1.5.0"
                "0dznvd1c5b02iilqm044q4hvar0sqibq1z46prqwjzwif61vpb0k"))

(define rust-icu-locid-transform-1.5.0
  (crate-source "icu_locid_transform" "1.5.0"
                "0kmmi1kmj9yph6mdgkc7v3wz6995v7ly3n80vbg0zr78bp1iml81"))

(define rust-icu-locid-transform-data-1.5.0
  (crate-source "icu_locid_transform_data" "1.5.0"
                "0vkgjixm0wzp2n3v5mw4j89ly05bg3lx96jpdggbwlpqi0rzzj7x"))

(define rust-icu-normalizer-1.5.0
  (crate-source "icu_normalizer" "1.5.0"
                "0kx8qryp8ma8fw1vijbgbnf7zz9f2j4d14rw36fmjs7cl86kxkhr"))

(define rust-icu-normalizer-data-1.5.0
  (crate-source "icu_normalizer_data" "1.5.0"
                "05lmk0zf0q7nzjnj5kbmsigj3qgr0rwicnn5pqi9n7krmbvzpjpq"))

(define rust-icu-properties-1.5.1
  (crate-source "icu_properties" "1.5.1"
                "1xgf584rx10xc1p7zjr78k0n4zn3g23rrg6v2ln31ingcq3h5mlk"))

(define rust-icu-properties-data-1.5.0
  (crate-source "icu_properties_data" "1.5.0"
                "0scms7pd5a7yxx9hfl167f5qdf44as6r3bd8myhlngnxqgxyza37"))

(define rust-icu-provider-1.5.0
  (crate-source "icu_provider" "1.5.0"
                "1nb8vvgw8dv2inqklvk05fs0qxzkw8xrg2n9vgid6y7gm3423m3f"))

(define rust-icu-provider-macros-1.5.0
  (crate-source "icu_provider_macros" "1.5.0"
                "1mjs0w7fcm2lcqmbakhninzrjwqs485lkps4hz0cv3k36y9rxj0y"))

(define rust-ident-case-1.0.1
  (crate-source "ident_case" "1.0.1"
                "0fac21q6pwns8gh1hz3nbq15j8fi441ncl6w4vlnd1cmc55kiq5r"))

(define rust-idna-1.0.3
  (crate-source "idna" "1.0.3"
                "0zlajvm2k3wy0ay8plr07w22hxkkmrxkffa6ah57ac6nci984vv8"))

(define rust-idna-adapter-1.2.0
  (crate-source "idna_adapter" "1.2.0"
                "0wggnkiivaj5lw0g0384ql2d7zk4ppkn3b1ry4n0ncjpr7qivjns"))

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

(define rust-jiff-0.1.28
  (crate-source "jiff" "0.1.28"
                "10as11fcrxa448lnzi6f3pi9mcax7957d8hiwv6zwr47w8lcf1y6"))

(define rust-jiff-tzdb-0.1.2
  (crate-source "jiff-tzdb" "0.1.2"
                "1lv0mb5ad182w5gkmb114h5v1ww9zfqlikhy0xdg8si6blpyqb6g"))

(define rust-jiff-tzdb-platform-0.1.2
  (crate-source "jiff-tzdb-platform" "0.1.2"
                "0zk9rb7b4xrdb3m1xlyhs4zziy57hpc548vrs9wjkfg70kj64g56"))

(define rust-jobserver-0.1.32
  (crate-source "jobserver" "0.1.32"
                "1l2k50qmj84x9mn39ivjz76alqmx72jhm12rw33zx9xnpv5xpla8"))

(define rust-lazy-static-1.5.0
  (crate-source "lazy_static" "1.5.0"
                "1zk6dqqni0193xg6iijh7i3i44sryglwgvx20spdvwk3r6sbrlmv"))

(define rust-libc-0.2.169
  (crate-source "libc" "0.2.169"
                "02m253hs8gw0m1n8iyrsc4n15yzbqwhddi7w1l0ds7i92kdsiaxm"))

(define rust-hermit-abi-0.3.0
  (crate-source "hermit-abi" "0.3.0"
                "009yjbdxhlpdnpm9h2vpir9wfw4v1yixz5zxsmjnsarcj2q5qsw5"))

(define rust-hermit-abi-0.1.20
  (crate-source "hermit-abi" "0.1.20"
                "1ypkwpf5r7m0jqdn2wfa0070i412kn9snvi1hg52w1yfvc40k8y7"))

(define rust-atty-0.2.14
  (crate-source "atty" "0.2.14"
                "1s7yslcs6a28c5vz7jwj63lkfgyx8mx99fdirlhi9lbhhzhrpcyr"))

(define rust-cast-0.3.0
  (crate-source "cast" "0.3.0"
                "1dbyngbyz2qkk0jn2sxil8vrz3rnpcj142y184p9l4nbl9radcip"))

(define rust-clap-2.34.0
  (crate-source "clap" "2.34.0"
                "071q5d8jfwbazi6zhik9xwpacx5i6kb2vkzy060vhf0c3120aqd0"))

(define rust-criterion-plot-0.4.5
  (crate-source "criterion-plot" "0.4.5"
                "0xhq0jz1603585h7xvm3s4x9irmifjliklszbzs4cda00y1cqwr6"))

(define rust-csv-1.1.6
  (crate-source "csv" "1.1.6"
                "1q9nqn0qlamwl18v57p82c8yhxy43lkzf2z1mndmycsvqinkm092"))

(define rust-csv-core-0.1.12
  (crate-source "csv-core" "0.1.12"
                "0gfrjjlfagarhyclxrqv6b14iaxgvgc8kmwwdvw08racvaqg60kx"))

(define rust-itertools-0.10.5
  (crate-source "itertools" "0.10.5"
                "0ww45h7nxx5kj6z2y6chlskxd1igvs4j507anr6dzg99x1h25zdh"))

(define rust-num-traits-0.2.19
  (crate-source "num-traits" "0.2.19"
                "0h984rhdkkqd4ny9cif7y2azl3xdfb7768hb9irhpsch4q3gq787"))

;; Use oorandom from main Guix
(define rust-oorandom-11.1.5
  (@@ (gnu packages rust-crates) rust-oorandom-11.1.5))

(define rust-plotters-backend-0.3.7
  (crate-source "plotters-backend" "0.3.7"
                "0ahpliim4hrrf7d4ispc2hwr7rzkn6d6nf7lyyrid2lm28yf2hnz"))

(define rust-plotters-svg-0.3.7
  (crate-source "plotters-svg" "0.3.7"
                "0w56sxaa2crpasa1zj0bhxzihlapqfkncggavyngg0w86anf5fji"))

;; Use plotters from main Guix
(define rust-plotters-0.3.7
  (@@ (gnu packages rust-crates) rust-plotters-0.3.7))

;; Use rayon from main Guix
(define rust-rayon-1.10.0
  (@@ (gnu packages rust-crates) rust-rayon-1.10.0))

;; Use serde from main Guix
(define rust-serde-1.0.219
  (@@ (gnu packages rust-crates) rust-serde-1.0.219))

;; Use common criterion dependencies from main Guix
(define rust-serde-cbor-0.11.2
  (@@ (gnu packages rust-crates) rust-serde-cbor-0.11.2))

(define rust-serde-derive-1.0.219
  (@@ (gnu packages rust-crates) rust-serde-derive-1.0.219))

(define rust-serde-json-1.0.140
  (@@ (gnu packages rust-crates) rust-serde-json-1.0.140))

(define rust-tinytemplate-1.2.1
  (@@ (gnu packages rust-crates) rust-tinytemplate-1.2.1))

(define rust-roff-0.2.2
  (@@ (gnu packages rust-crates) rust-roff-0.2.2))

;; More clap dependencies from main Guix
(define rust-anstyle-1.0.11
  (@@ (gnu packages rust-crates) rust-anstyle-1.0.11))

(define rust-anstyle-parse-0.2.7
  (@@ (gnu packages rust-crates) rust-anstyle-parse-0.2.7))

(define rust-strsim-0.11.1
  (@@ (gnu packages rust-crates) rust-strsim-0.11.1))

(define rust-utf8parse-0.2.2
  (@@ (gnu packages rust-crates) rust-utf8parse-0.2.2))

(define rust-clap-lex-0.7.5
  (@@ (gnu packages rust-crates) rust-clap-lex-0.7.5))

(define rust-proc-macro2-1.0.101
  (@@ (gnu packages rust-crates) rust-proc-macro2-1.0.101))

(define rust-quote-1.0.40
  (@@ (gnu packages rust-crates) rust-quote-1.0.40))

(define rust-syn-2.0.106
  (@@ (gnu packages rust-crates) rust-syn-2.0.106))

(define rust-bstr-1.12.0
  (@@ (gnu packages rust-crates) rust-bstr-1.12.0))

(define rust-bstr-0.2.17
  (crate-source "bstr" "0.2.17"
                "08rjbhysy6gg27db2h3pnhvr2mlr5vkj797i9625kwg8hgrnjdds"))

(define rust-bytecount-0.6.9
  (crate-source "bytecount" "0.6.9"
                "0pinq0n8zza8qr2lyc3yf17k963129kdbf0bwnmvdk1bpvh14n0p"))

(define rust-encoding-rs-0.8.35
  (crate-source "encoding_rs" "0.8.35"
                "1wv64xdrr9v37rqqdjsyb8l8wzlcbab80ryxhrszvnj59wy0y0vm"))

(define rust-encoding-rs-io-0.1.7
  (crate-source "encoding_rs_io" "0.1.7"
                "10ra4l688cdadd8h1lsbahld1zbywnnqv68366mbhamn3xjwbhqw"))

(define rust-memmap2-0.9.8
  (crate-source "memmap2" "0.9.8"
                "1dqxjs89krh8cin0k7ksqc9myw9yni9kn8d8cllwq4fn1isrhfl4"))

(define rust-memmap2-0.5.10
  (crate-source "memmap2" "0.5.10"
                "09xk415fxyl4a9pgby4im1v2gqlb5lixpm99dczkk30718na9yl3"))

(define rust-crossbeam-epoch-0.9.18
  (crate-source "crossbeam-epoch" "0.9.18"
                "03j2np8llwf376m3fxqx859mgp9f83hj1w34153c7a9c7i5ar0jv"))

(define rust-winapi-0.3.9
  (crate-source "winapi" "0.3.9"
                "06gl025x418lchw1wxj64ycr7gha83m44cjr5sarhynd9xkrm0sw"))

(define rust-syn-1.0.109
  (crate-source "syn" "1.0.109"
                "0ds2if4600bd59wsv7jjgfkayfzy3hnazs394kz6zdkmna8l3dkj"))

(define rust-bitflags-2.9.4
  (crate-source "bitflags" "2.9.4"
                "157kkcv8s7vk6d17dar1pa5cqcz4c8pdrn16wm1ld7jnr86d2q92"))

(define rust-base64-0.20.0
  (@@ (gnu packages rust-crates) rust-base64-0.20.0))

(define rust-fastrand-2.2.0
  (crate-source "fastrand" "2.2.0"
                "1i0sp22gv8n4h4w5cf10l2b3rfdi0da2kp0d4hl7jw65fdp80vs8"))

(define rust-once-cell-1.20.2
  (crate-source "once_cell" "1.20.2"
                "0xb7rw1aqr7pa4z3b00y7786gyf8awx2gca3md73afy76dzgwq8j"))

(define rust-rustix-0.38.42
  (crate-source "rustix" "0.38.42"
                "11fvprv3p450ggyqacp7sdpjbbsgm5zvfjwnzy8bfbmbrf7c6ggr"))

(define rust-windows-sys-0.59.0
  (crate-source "windows-sys" "0.59.0"
                "0fw5672ziw8b3zpmnbp9pdv1famk74f1l9fcbc3zsrzdg56vqf0y"))

(define rust-libgit2-sys-0.18.0+1.9.0
  ;; TODO: Check bundled sources.
  (crate-source "libgit2-sys" "0.18.0+1.9.0"
                "1v7zcw1kky338grxs70y7fwpy70g846bpa5yzvl9f5bybr31g8g1"))

(define rust-libredox-0.1.3
  (crate-source "libredox" "0.1.3"
                "139602gzgs0k91zb7dvgj1qh4ynb8g1lbxsswdim18hcb6ykgzy0"))

(define rust-libssh2-sys-0.3.0
  ;; TODO: Check bundled sources.
  (crate-source "libssh2-sys" "0.3.0"
                "1vkidqw5ll71ynqc93hgcq62iqkklzb5268zffd13ql7nwqa1j1d"))

(define rust-libz-sys-1.1.20
  ;; TODO: Check bundled sources.
  (crate-source "libz-sys" "1.1.20"
                "0wp4i6zl385ilmcqafv61jwsk1mpk6yb8gpws9nwza00x19n9lfj"))

(define rust-linux-raw-sys-0.4.14
  ;; TODO: Check bundled sources.
  (crate-source "linux-raw-sys" "0.4.14"
                "12gsjgbhhjwywpqcrizv80vrp7p7grsz5laqq773i33wphjsxcvq"))

(define rust-litemap-0.7.4
  (crate-source "litemap" "0.7.4"
                "012ili3vppd4952sh6y3qwcd0jkd0bq2qpr9h7cppc8sj11k7saf"))

(define rust-log-0.4.22
  (crate-source "log" "0.4.22"
                "093vs0wkm1rgyykk7fjbqp2lwizbixac1w52gv109p5r4jh0p9x7"))

(define rust-memchr-2.7.4
  (crate-source "memchr" "2.7.4"
                "18z32bhxrax0fnjikv475z7ii718hq457qwmaryixfxsl2qrmjkq"))

(define rust-miniz-oxide-0.8.2
  (crate-source "miniz_oxide" "0.8.2"
                "1543asrvhla92sby4z6m9ilkg2cmmq8ja6bj84k1vp6f48qfiysg"))

(define rust-nix-0.29.0
  (crate-source "nix" "0.29.0"
                "0ikvn7s9r2lrfdm3mx1h7nbfjvcc6s9vxdzw7j5xfkd2qdnp9qki"))

(define rust-num-conv-0.1.0
  (crate-source "num-conv" "0.1.0"
                "1ndiyg82q73783jq18isi71a7mjh56wxrk52rlvyx0mi5z9ibmai"))

(define rust-num-modular-0.6.1
  (crate-source "num-modular" "0.6.1"
                "0zv4miws3q1i93a0bd9wgc4njrr5j5786kr99hzxi9vgycdjdfqp"))

(define rust-num-order-1.2.0
  (crate-source "num-order" "1.2.0"
                "1dhvdncf91ljxh9sawnfxcbiqj1gnag08lyias0cy3y4jxmmjysk"))

(define rust-object-0.36.7
  (crate-source "object" "0.36.7"
                "11vv97djn9nc5n6w1gc6bd96d2qk2c8cg1kw5km9bsi3v4a8x532"))

(define rust-openssl-sys-0.9.104
  ;; TODO: Check bundled sources.
  (crate-source "openssl-sys" "0.9.104"
                "0hf712xcxmycnlc09r8d446b3mwqchsbfrjv374fp7grrc3g7as5"))

(define rust-option-ext-0.2.0
  (crate-source "option-ext" "0.2.0"
                "0zbf7cx8ib99frnlanpyikm1bx8qn8x602sw1n7bg6p9x94lyx04"))

(define rust-percent-encoding-2.3.1
  (crate-source "percent-encoding" "2.3.1"
                "0gi8wgx0dcy8rnv1kywdv98lwcx67hz0a0zwpib5v2i08r88y573"))

(define rust-pest-2.7.15
  (crate-source "pest" "2.7.15"
                "1p4rq45xprw9cx0pb8mmbfa0ih49l0baablv3cpfdy3c1pkayz4b"))

(define rust-pest-derive-2.7.15
  (crate-source "pest_derive" "2.7.15"
                "0zpmcd1jv1c53agad5b3jb66ylxlzyv43x1bssh8fs7w3i11hrc1"))

(define rust-pest-generator-2.7.15
  (crate-source "pest_generator" "2.7.15"
                "0yrpk5ymc56pffv7gqr5rkv92p3dc6s73lb8hy1wf3w77byrc4vx"))

(define rust-pest-meta-2.7.15
  (crate-source "pest_meta" "2.7.15"
                "1skx7gm932bp77if63f7d72jrk5gygj39d8zsfzigmr5xa4q1rg1"))

(define rust-pkg-config-0.3.31
  (crate-source "pkg-config" "0.3.31"
                "1wk6yp2phl91795ia0lwkr3wl4a9xkrympvhqq8cxk4d75hwhglm"))

(define rust-portable-atomic-1.10.0
  (crate-source "portable-atomic" "1.10.0"
                "1rjfim62djiakf5rcq3r526hac0d1dd9hwa1jmiin7q7ad2c4398"))

(define rust-portable-atomic-util-0.2.4
  (crate-source "portable-atomic-util" "0.2.4"
                "01rmx1li07ixsx3sqg2bxqrkzk7b5n8pibwwf2589ms0s3cg18nq"))

(define rust-powerfmt-0.2.0
  (crate-source "powerfmt" "0.2.0"
                "14ckj2xdpkhv3h6l5sdmb9f1d57z8hbfpdldjc2vl5givq2y77j3"))

(define rust-proc-macro2-1.0.92
  (crate-source "proc-macro2" "1.0.92"
                "1c1vjy5wg8iy7kxsxda564qf4ljp0asysmbn2i7caj177x5m9lrp"))

(define rust-quote-1.0.37
  (crate-source "quote" "1.0.37"
                "1brklraw2g34bxy9y4q1nbrccn7bv36ylihv12c9vlcii55x7fdm"))

(define rust-quote-1.0.38
  (crate-source "quote" "1.0.38"
                "1k0s75w61k6ch0rs263r4j69b7vj1wadqgb9dia4ylc9mymcqk8f"))

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

(define rust-rustc-demangle-0.1.24
  (crate-source "rustc-demangle" "0.1.24"
                "07zysaafgrkzy2rjgwqdj2a8qdpsm6zv6f5pgpk9x0lm40z9b6vi"))

(define rust-rustversion-1.0.18
  (crate-source "rustversion" "1.0.18"
                "0j2207vmgrcxwwwvknfn3lwv4i8djhjnxlvwdnz8bwijqqmrz08f"))

(define rust-ryu-1.0.18
  (crate-source "ryu" "1.0.18"
                "17xx2s8j1lln7iackzd9p0sv546vjq71i779gphjq923vjh5pjzk"))

(define rust-same-file-1.0.6
  (crate-source "same-file" "1.0.6"
                "00h5j1w87dmhnvbv9l8bic3y7xxsnjmssvifw2ayvgx9mb1ivz4k"))

(define rust-semver-1.0.25
  (crate-source "semver" "1.0.25"
                "00sy306qpi7vfand7dxm2vc76nlc8fkh1rrhdy0qh12v50nzx7gp"))

(define rust-serde-1.0.216
  (crate-source "serde" "1.0.216"
                "13ikqs0cvd220530x4rj1m9ab5wcflrwkw7cpvl9fnlkdq0q35qb"))

(define rust-serde-1.0.217
  (crate-source "serde" "1.0.217"
                "0w2ck1p1ajmrv1cf51qf7igjn2nc51r0izzc00fzmmhkvxjl5z02"))

(define rust-serde-derive-1.0.216
  (crate-source "serde_derive" "1.0.216"
                "0pm5bm4354n40ir12bbs829arlqwjrw0wmzd4xk5r1kkpzdmky26"))

(define rust-serde-derive-1.0.217
  (crate-source "serde_derive" "1.0.217"
                "180r3rj5gi5s1m23q66cr5wlfgc5jrs6n1mdmql2njnhk37zg6ss"))

(define rust-serde-json-1.0.134
  (crate-source "serde_json" "1.0.134"
                "0z8wk61rzpqjmnwhv6k9zikhsfmsb6lr6qbg84aqpr1fqisl23yh"))

(define rust-serde-norway-0.9.42
  (crate-source "serde_norway" "0.9.42"
                "130nx1r3nwydglq1yrrcydavd6w5zj219zsimc7m1zdmi6ag4274"))

(define rust-serde-regex-1.1.0
  (crate-source "serde_regex" "1.1.0"
                "1pxsnxb8c198szghk1hvzvhva36w2q5zs70hqkmdf5d89qd6y4x8"))

(define rust-serde-yaml-0.9.34+deprecated
  (crate-source "serde_yaml" "0.9.34+deprecated"
                "0isba1fjyg3l6rxk156k600ilzr8fp7crv82rhal0rxz5qd1m2va"))

(define rust-sha2-0.10.8
  (crate-source "sha2" "0.10.8"
                "1j1x78zk9il95w9iv46dh9wm73r6xrgj32y6lzzw7bxws9dbfgbr"))

(define rust-shell-words-1.1.0
  (crate-source "shell-words" "1.1.0"
                "1plgwx8r0h5ismbbp6cp03740wmzgzhip85k5hxqrrkaddkql614"))

(define rust-shellexpand-3.1.0
  (crate-source "shellexpand" "3.1.0"
                "0jz1i14ziz8gbyj71212s7dqrw6q96f25i48zkmy66fcjhxzl0ys"))

(define rust-shlex-1.3.0
  (crate-source "shlex" "1.3.0"
                "0r1y6bv26c1scpxvhg2cabimrmwgbp4p3wy6syj9n0c4s3q2znhg"))

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

(define rust-smallvec-1.13.2
  (crate-source "smallvec" "1.13.2"
                "0rsw5samawl3wsw6glrsb127rx6sh89a8wyikicw6dkdcjd1lpiw"))

(define rust-stable-deref-trait-1.2.0
  (crate-source "stable_deref_trait" "1.2.0"
                "1lxjr8q2n534b2lhkxd6l6wcddzjvnksi58zv11f9y0jjmr15wd8"))

(define rust-structdoc-0.1.4
  (crate-source "structdoc" "0.1.4"
                "04bzjwlg8cxfbqgmg2i3s5y0lgmcsdj173byix2sa3dlf6955n4g"))

(define rust-structdoc-derive-0.1.4
  (crate-source "structdoc-derive" "0.1.4"
                "1yjdi987jaqbypfanyllldk6ww2vswniniavn3pb4zrpazc75ah1"))

(define rust-syn-2.0.91
  (crate-source "syn" "2.0.91"
                "0df08gvjksnnapcqcbc72l44jlr1r957y7cbhnvk7ga3lasvqg6m"))

(define rust-syn-2.0.93
  (crate-source "syn" "2.0.93"
                "0n6hk0yipq1q6cc8wb9jhw54l9vlvwiyc0182fqns3gfv9i60y4w"))

(define rust-synstructure-0.13.1
  (crate-source "synstructure" "0.13.1"
                "0wc9f002ia2zqcbj0q2id5x6n7g1zjqba7qkg2mr0qvvmdk7dby8"))

(define rust-syslog-5.0.0
  (crate-source "syslog" "5.0.0"
                "0paii62qnwjnfliygdal1x3hqxjkci1nlczfydv7kh3rnvqqwpcs"))

(define rust-take-mut-0.2.2
  (crate-source "take_mut" "0.2.2"
                "0q2d7w6nd5bl7bay5csq065sjg8fw0jcx6hl1983cpzf25fh0r7p"))

(define rust-tempfile-3.14.0
  (crate-source "tempfile" "3.14.0"
                "037f9jm13bmfc6xq9w86dp0nylrddh6ynvl6db4gm1xwzi8y5k18"))

(define rust-term-0.7.0
  (crate-source "term" "0.7.0"
                "07xzxmg7dbhlirpyfq09v7cfb9gxn0077sqqvszgjvyrjnngi7f5"))

(define rust-thiserror-1.0.69
  (crate-source "thiserror" "1.0.69"
                "0lizjay08agcr5hs9yfzzj6axs53a2rgx070a1dsi3jpkcrzbamn"))

(define rust-thiserror-2.0.11
  (crate-source "thiserror" "2.0.11"
                "1z0649rpa8c2smzx129bz4qvxmdihj30r2km6vfpcv9yny2g4lnl"))

(define rust-thiserror-impl-1.0.69
  (crate-source "thiserror-impl" "1.0.69"
                "1h84fmn2nai41cxbhk6pqf46bxqq1b344v8yz089w1chzi76rvjg"))

(define rust-thiserror-impl-2.0.11
  (crate-source "thiserror-impl" "2.0.11"
                "1hkkn7p2y4cxbffcrprybkj0qy1rl1r6waxmxqvr764axaxc3br6"))

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

(define rust-tinystr-0.7.6
  (crate-source "tinystr" "0.7.6"
                "0bxqaw7z8r2kzngxlzlgvld1r6jbnwyylyvyjbv1q71rvgaga5wi"))

(define rust-typenum-1.17.0
  (crate-source "typenum" "1.17.0"
                "09dqxv69m9lj9zvv6xw5vxaqx15ps0vxyy5myg33i0kbqvq0pzs2"))

(define rust-ucd-trie-0.1.7
  (crate-source "ucd-trie" "0.1.7"
                "0wc9p07sqwz320848i52nvyjvpsxkx3kv5bfbmm6s35809fdk5i8"))

(define rust-unicode-ident-1.0.14
  (crate-source "unicode-ident" "1.0.14"
                "10ywa1pg0glgkr4l3dppjxizr9r2b7im0ycbfa0137l69z5fdfdd"))

(define rust-unicode-segmentation-1.12.0
  (crate-source "unicode-segmentation" "1.12.0"
                "14qla2jfx74yyb9ds3d2mpwpa4l4lzb9z57c6d2ba511458z5k7n"))

(define rust-unicode-width-0.2.0
  (crate-source "unicode-width" "0.2.0"
                "1zd0r5vs52ifxn25rs06gxrgz8cmh4xpra922k0xlmrchib1kj0z"))

(define rust-unindent-0.1.11
  (crate-source "unindent" "0.1.11"
                "171may3v15wzc10z64i8sahdz49d031v7424mjsifa205ml6sxp1"))

(define rust-unsafe-libyaml-0.2.11
  (crate-source "unsafe-libyaml" "0.2.11"
                "0qdq69ffl3v5pzx9kzxbghzn0fzn266i1xn70y88maybz9csqfk7"))

(define rust-unsafe-libyaml-norway-0.2.15
  (crate-source "unsafe-libyaml-norway" "0.2.15"
                "0111lbq845fwqv8cn89m02v7bjd2lq2jvd814dziqlijpxcvv6mk"))

(define rust-url-2.5.4
  (crate-source "url" "2.5.4"
                "0q6sgznyy2n4l5lm16zahkisvc9nip9aa5q1pps7656xra3bdy1j"))

(define rust-utf16-iter-1.0.5
  (crate-source "utf16_iter" "1.0.5"
                "0ik2krdr73hfgsdzw0218fn35fa09dg2hvbi1xp3bmdfrp9js8y8"))

(define rust-utf8-iter-1.0.4
  (crate-source "utf8_iter" "1.0.4"
                "1gmna9flnj8dbyd8ba17zigrp9c4c3zclngf5lnb5yvz1ri41hdn"))

(define rust-vcpkg-0.2.15
  (crate-source "vcpkg" "0.2.15"
                "09i4nf5y8lig6xgj3f7fyrvzd3nlaw4znrihw8psidvv5yk4xkdc"))

(define rust-version-check-0.9.5
  (crate-source "version_check" "0.9.5"
                "0nhhi4i5x89gm911azqbn7avs9mdacw2i3vcz3cnmz3mv4rqz4hb"))

(define rust-walkdir-2.5.0
  (crate-source "walkdir" "2.5.0"
                "0jsy7a710qv8gld5957ybrnc07gavppp963gs32xk4ag8130jy99"))

(define rust-wasi-0.10.0+wasi-snapshot-preview1
  (crate-source "wasi" "0.10.0+wasi-snapshot-preview1"
                "07y3l8mzfzzz4cj09c8y90yak4hpsi9g7pllyzpr6xvwrabka50s"))

(define rust-wasi-0.11.0+wasi-snapshot-preview1
  (crate-source "wasi" "0.11.0+wasi-snapshot-preview1"
                "08z4hxwkpdpalxjps1ai9y7ihin26y9f476i53dv98v45gkqg3cw"))

(define rust-winapi-i686-pc-windows-gnu-0.4.0
  (crate-source "winapi-i686-pc-windows-gnu" "0.4.0"
                "1dmpa6mvcvzz16zg6d5vrfy4bxgg541wxrcip7cnshi06v38ffxc"))

(define rust-winapi-util-0.1.9
  (crate-source "winapi-util" "0.1.9"
                "1fqhkcl9scd230cnfj8apfficpf5c9vhwnk4yy9xfc1sw69iq8ng"))

(define rust-winapi-x86-64-pc-windows-gnu-0.4.0
  (crate-source "winapi-x86_64-pc-windows-gnu" "0.4.0"
                "0gqq64czqb64kskjryj8isp62m2sgvx25yyj3kpc2myh85w24bki"))

(define rust-windows-sys-0.48.0
  (crate-source "windows-sys" "0.48.0"
                "1aan23v5gs7gya1lc46hqn9mdh8yph3fhxmhxlw36pn6pqc28zb7"))

(define rust-windows-sys-0.52.0
  (crate-source "windows-sys" "0.52.0"
                "0gd3v4ji88490zgb6b5mq5zgbvwv7zx1ibn8v3x83rwcdbryaar8"))

(define rust-windows-targets-0.48.5
  (crate-source "windows-targets" "0.48.5"
                "034ljxqshifs1lan89xwpcy1hp0lhdh4b5n0d2z4fwjx2piacbws"))

(define rust-windows-targets-0.52.6
  (crate-source "windows-targets" "0.52.6"
                "0wwrx625nwlfp7k93r2rra568gad1mwd888h1jwnl0vfg5r4ywlv"))

(define rust-windows-targets-0.53.4
  (crate-source "windows-targets" "0.53.4"
                "0jxc6f032xb3bbb7mj9rlhky84w7vz7hkbsh8s2hcakdysvvfhid"))

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

(define rust-write16-1.0.0
  (crate-source "write16" "1.0.0"
                "0dnryvrrbrnl7vvf5vb1zkmwldhjkf2n5znliviam7bm4900z2fi"))

(define rust-writeable-0.5.5
  (crate-source "writeable" "0.5.5"
                "0lawr6y0bwqfyayf3z8zmqlhpnzhdx0ahs54isacbhyjwa7g778y"))

(define rust-ws2-32-sys-0.2.1
  (crate-source "ws2_32-sys" "0.2.1"
                "0ppscg5qfqaw0gzwv2a4nhn5bn01ff9iwn6ysqnzm4n8s3myz76m"))

;; Additional dependencies for ripsecrets
(define rust-grep-0.2
  (crate-source "grep" "0.2.12"
                "07hpnvi9yq8j3ndhdfmbh8iklqyyir3alv3ljp8f9qr0k0p384yr"))

(define rust-grep-cli-0.1
  (crate-source "grep-cli" "0.1.7"
                "18bvb92w4dx6lsfx315nzrgv1g00qzr29k9rm0cnfjy6gdlcd7yi"))

(define rust-grep-matcher-0.1
  (crate-source "grep-matcher" "0.1.7"
                "00mcjar5b6y1pwf0gjdywzgh1fnp6jl612n9qznwyfm420d198s7"))

(define rust-grep-printer-0.1
  (crate-source "grep-printer" "0.1.7"
                "0sjsv4v7a88arckc880ns3mbn2y2p38mpnkh9dviznj9ixbm2ig1"))

(define rust-grep-regex-0.1
  (crate-source "grep-regex" "0.1.13"
                "0zhzz5hffx30ff7xi3p4m3amg5c4rr2i7a6kdfifg5ijgry19pcy"))

(define rust-grep-searcher-0.1
  (crate-source "grep-searcher" "0.1.11"
                "17vpb263c52jdix968wj6v2m12kv8jznq7qbnkmwkw40yjww80an"))

(define rust-crossbeam-deque-0.8
  (crate-source "crossbeam-deque" "0.8.6"
                "0l9f1saqp1gn5qy0rxvkmz4m6n7fc0b3dbm6q1r5pmgpnyvi3lcx"))

(define rust-globset-0.4
  (crate-source "globset" "0.4.16"
                "1xa9ivqs74imf1q288spxh49g6iw2mn3x9snibdgapazzj6h58al"))

(define rust-log-0.4
  (crate-source "log" "0.4.20"
                "13rf7wphnwd61vazpxr7fiycin6cb1g8fmvgqg18i464p0y1drmm"))

(define rust-memchr-2.6
  (crate-source "memchr" "2.6.3"
                "0p6kn2awqf47m3brk0nmajarhwhylg9969il8dm9bq87yxp2s8wg"))

(define rust-regex-automata-0.4
  (crate-source "regex-automata" "0.4.8"
                "18wd530ndrmygi6xnz3sp345qi0hy2kdbsa89182nwbl6br5i1rn"))

(define rust-same-file-1.0
  (crate-source "same-file" "1.0.6"
                "00h5j1w87dmhnvbv9l8bic3y7xxsnjmssvifw2ayvgx9mb1ivz4k"))

(define rust-walkdir-2.4
  (crate-source "walkdir" "2.4.0"
                "1vjl9fmfc4v8k9ald23qrpcbyb8dl1ynyq8d516cm537r1yqa7fp"))

(define rust-ignore-0.4
  (crate-source "ignore" "0.4.23"
                "0jysggjfmlxbg60vhhiz4pb8jfb7cnq5swdsvxknbs7x18wgv2bd"))

(define rust-regex-1
  (crate-source "regex" "1.11.1"
                "148i41mzbx8bmq32hsj1q4karkzzx5m60qza6gdw4pdc9qdyyi5m"))

(define rust-tempfile-3
  (crate-source "tempfile" "3.14.0"
                "037f9jm13bmfc6xq9w86dp0nylrddh6ynvl6db4gm1xwzi8y5k18"))

(define rust-termcolor-1
  (crate-source "termcolor" "1.4.1"
                "0mappjh3fj3p2nmrg4y7qv94rchwi9mzmgmfflr8p2awdj7lyy86"))

(define rust-num-cpus-1
  (crate-source "num_cpus" "1.16.0"
                "0hra6ihpnh06dvfvz9ipscys0xfqa9ca9hzp384d5m02ssvgqqa1"))

(define rust-memoize-0.3
  (crate-source "memoize" "0.3.2"
                "1gb01fri98whmb2kivq7yjhyab0iqya67pipwqs0fa5pvlfx2lg5"))

(define rust-memoize-inner-0.2
  (crate-source "memoize-inner" "0.2.1"
                "18zq2chd872ixwmcny5kr58f203pdyrdz8l7vjj6783bb1ziwyi1"))

(define rust-lazy-static-1
  (crate-source "lazy_static" "1.5.0"
                "1zk6dqqni0193xg6iijh7i3i44sryglwgvx20spdvwk3r6sbrlmv"))

;; Dummy criterion to satisfy build requirements
(define rust-criterion-0.3
  (crate-source "criterion" "0.3.6"
                "13yd64ah93gkbdv7qq4cr6rhgl9979jjcjk3gkhnav1b7glns7dh"))

(define rust-clap-complete-4.5
  (crate-source "clap_complete" "4.5.43"
                "0q8q8qsk3f6c1g5wk05i5836qp3rbdjh45j91ykdrin98lsh2lh9"))

(define rust-clap-mangen-0.2
  (crate-source "clap_mangen" "0.2.15"
                "1xvpgl7rzw2jglvkwizwgf098k9xd8s7b3zm5qfiwsa70418dgnk"))

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

(define rust-env-logger-0.9.3
  (crate-source "env_logger" "0.9.3"
                "1rq0kqpa8my6i1qcyhfqrn1g9xr5fbkwwbd42nqvlzn9qibncbm1"))

(define rust-hashbrown-0.12.3
  (crate-source "hashbrown" "0.12.3"
                "1268ka4750pyg2pbgsr43f0289l5zah4arir2k4igx5a8c6fg7la"))

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

(define rust-serde-json-1.0.143
  (crate-source "serde_json" "1.0.143"
                "0njabwzldvj13ykrf1aaf4gh5cgl25kf9hzbpafbv3qh3ppsn0fl"))

(define rust-serde-yaml-0.8.26
  (crate-source "serde_yaml" "0.8.26"
                "06y7gxy312mink8nsnmci9cw0ykpgsdcxmayg0snmdbnnwrp92jp"))

(define rust-structopt-0.3.26
  (crate-source "structopt" "0.3.26"
                "043sg3qxllann6q9i71d05qp3q13scmcvhxhd950ka2v8ij5qsqc"))

(define rust-structopt-derive-0.4.18
  (crate-source "structopt-derive" "0.4.18"
                "1q5gcigmvw0cinjxzpyrkflliq5r1ivljmrvfrl3phcwgwraxdfw"))

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

(define rust-yoke-0.7.5
  (crate-source "yoke" "0.7.5"
                "0h3znzrdmll0a7sglzf9ji0p5iqml11wrj1dypaf6ad6kbpnl3hj"))

(define rust-yoke-derive-0.7.5
  (crate-source "yoke-derive" "0.7.5"
                "0m4i4a7gy826bfvnqa9wy6sp90qf0as3wps3wb0smjaamn68g013"))

(define rust-zerofrom-0.1.5
  (crate-source "zerofrom" "0.1.5"
                "0bnd8vjcllzrvr3wvn8x14k2hkrpyy1fm3crkn2y3plmr44fxwyg"))

(define rust-zerofrom-derive-0.1.5
  (crate-source "zerofrom-derive" "0.1.5"
                "022q55phhb44qbrcfbc48k0b741fl8gnazw3hpmmndbx5ycfspjr"))

(define rust-zeroize-1.8.1
  (crate-source "zeroize" "1.8.1"
                "1pjdrmjwmszpxfd7r860jx54cyk94qk59x13sc307cvr5256glyf"))

(define rust-zerovec-0.10.4
  (crate-source "zerovec" "0.10.4"
                "0yghix7n3fjfdppwghknzvx9v8cf826h2qal5nqvy8yzg4yqjaxa"))

(define rust-zerovec-derive-0.10.3
  (crate-source "zerovec-derive" "0.10.3"
                "1ik322dys6wnap5d3gcsn09azmssq466xryn5czfm13mn7gsdbvf"))

;; Crate sources for voice-type dependencies

(define rust-adler2-2.0.1
  (crate-source "adler2" "2.0.1"
                "1ymy18s9hs7ya1pjc9864l30wk8p2qfqdi7mhhcc5nfakxbij09j"))

(define rust-aho-corasick-1.1.4
  (crate-source "aho-corasick" "1.1.4"
                "00a32wb2h07im3skkikc495jvncf62jl6s96vwc7bhi70h9imlyx"))

(define rust-alsa-0.9.1
  (crate-source "alsa" "0.9.1"
                "0hvxc447bsynyhzhmznw6w2kwbid83p712dls4h1x8w3pavp4xgd"))

(define rust-alsa-sys-0.3.1
  (crate-source "alsa-sys" "0.3.1"
                "09qmmnpmlcj23zcgx2xsi4phcgm5i02g9xaf801y7i067mkfx3yv"))

(define rust-anstream-0.6.21
  (crate-source "anstream" "0.6.21"
                "0jjgixms4qjj58dzr846h2s29p8w7ynwr9b9x6246m1pwy0v5ma3"))

(define rust-anstyle-1.0.13
  (crate-source "anstyle" "1.0.13"
                "0y2ynjqajpny6q0amvfzzgw0gfw3l47z85km4gvx87vg02lcr4ji"))

(define rust-anstyle-query-1.1.5
  (crate-source "anstyle-query" "1.1.5"
                "1p6shfpnbghs6jsa0vnqd8bb8gd7pjd0jr7w0j8jikakzmr8zi20"))

(define rust-anstyle-wincon-3.0.11
  (crate-source "anstyle-wincon" "3.0.11"
                "0zblannm70sk3xny337mz7c6d8q8i24vhbqi42ld8v7q1wjnl7i9"))

(define rust-anyhow-1.0.102
  (crate-source "anyhow" "1.0.102"
                "0b447dra1v12z474c6z4jmicdmc5yxz5bakympdnij44ckw2s83z"))

(define rust-base64-0.22.1
  (crate-source "base64" "0.22.1"
                "1imqzgh7bxcikp5vx3shqvw9j09g9ly0xr0jma0q66i52r7jbcvj"))

(define rust-bindgen-0.72.1
  (crate-source "bindgen" "0.72.1"
                "15bq73y3wd3x3vxh3z3g72hy08zs8rxg1f0i1xsrrd6g16spcdwr"))

(define rust-bit-set-0.8.0
  (crate-source "bit-set" "0.8.0"
                "18riaa10s6n59n39vix0cr7l2dgwdhcpbcm97x1xbyfp1q47x008"))

(define rust-bit-vec-0.8.0
  (crate-source "bit-vec" "0.8.0"
                "1xxa1s2cj291r7k1whbxq840jxvmdsq9xgh7bvrxl46m80fllxjy"))

(define rust-bitflags-2.11.0
  (crate-source "bitflags" "2.11.0"
                "1bwjibwry5nfwsfm9kjg2dqx5n5nja9xymwbfl6svnn8jsz6ff44"))

(define rust-bitvec-1.0.1
  (crate-source "bitvec" "1.0.1"
                "173ydyj2q5vwj88k6xgjnfsshs4x9wbvjjv7sm0h36r34hn87hhv"))

(define rust-block-0.1.6
  (crate-source "block" "0.1.6"
                "16k9jgll25pzsq14f244q22cdv0zb4bqacldg3kx6h89d7piz30d"))

(define rust-bumpalo-3.20.2
  (crate-source "bumpalo" "3.20.2"
                "1jrgxlff76k9glam0akhwpil2fr1w32gbjdf5hpipc7ld2c7h82x"))

(define rust-bytes-1.11.1
  (crate-source "bytes" "1.11.1"
                "0czwlhbq8z29wq0ia87yass2mzy1y0jcasjb8ghriiybnwrqfx0y"))

(define rust-cairo-rs-0.20.12
  (crate-source "cairo-rs" "0.20.12"
                "1l5d1rgvagvvs4a99i28ciyhdygf9v8hhy8mpk5akbr59q7vvqwi"))

(define rust-cairo-sys-rs-0.20.10
  (crate-source "cairo-sys-rs" "0.20.10"
                "12sgv9mimxy5nsxm4ipga1k7an59wn444xa7kbywp64qai3cg705"))

(define rust-cc-1.2.56
  (crate-source "cc" "1.2.56"
                "1chvh9g2izhqad7vzy4cc7xpdljdvqpsr6x6hv1hmyqv3mlkbgxf"))

(define rust-cesu8-1.1.0
  (crate-source "cesu8" "1.1.0"
                "0g6q58wa7khxrxcxgnqyi9s1z2cjywwwd3hzr5c55wskhx6s0hvd"))

(define rust-cexpr-0.6.0
  (crate-source "cexpr" "0.6.0"
                "0rl77bwhs5p979ih4r0202cn5jrfsrbgrksp40lkfz5vk1x3ib3g"))

(define rust-cfg-expr-0.20.6
  (crate-source "cfg-expr" "0.20.6"
                "0smbxbd39s2kpmz6r9yg4xmh0wx5d1in6amf49rpr0m6l6szbkkq"))

(define rust-cfg-if-1.0.4
  (crate-source "cfg-if" "1.0.4"
                "008q28ajc546z5p2hcwdnckmg0hia7rnx52fni04bwqkzyrghc4k"))

(define rust-clang-sys-1.8.1
  (crate-source "clang-sys" "1.8.1"
                "1x1r9yqss76z8xwpdanw313ss6fniwc1r7dzb5ycjn0ph53kj0hb"))

(define rust-clap-4.5.60
  (crate-source "clap" "4.5.60"
                "02h3nzznssjgp815nnbzk0r62y2iw03kdli75c233kirld6z75r7"))

(define rust-clap-builder-4.5.60
  (crate-source "clap_builder" "4.5.60"
                "0xk8mdizvmmn6w5ij5cwhy5pbgyac4w9pfvl6nqmjl7a5hql38i4"))

(define rust-clap-derive-4.5.55
  (crate-source "clap_derive" "4.5.55"
                "1r949xis3jmhzh387smd70vc8a3b9734ck3g5ahg59a63bd969x9"))

(define rust-clap-lex-1.0.0
  (crate-source "clap_lex" "1.0.0"
                "0c8888qi1l9sayqlv666h8s0yxn2qc6jr88v1zagk43mpjjjx0is"))

(define rust-cocoa-0.22.0
  (crate-source "cocoa" "0.22.0"
                "19qyyv01yzrm6aahn6cdxvb4jhl6v4fj0cgqkxmq38i7hq3dqzv6"))

(define rust-colorchoice-1.0.4
  (crate-source "colorchoice" "1.0.4"
                "0x8ymkz1xr77rcj1cfanhf416pc4v681gmkc9dzb3jqja7f62nxh"))

(define rust-combine-4.6.7
  (crate-source "combine" "4.6.7"
                "1z8rh8wp59gf8k23ar010phgs0wgf5i8cx4fg01gwcnzfn5k0nms"))

(define rust-cookie-0.18.1
  (crate-source "cookie" "0.18.1"
                "0iy749flficrlvgr3hjmf3igr738lk81n5akzf4ym4cs6cxg7pjd"))

(define rust-cookie-store-0.22.1
  (crate-source "cookie_store" "0.22.1"
                "01jjqwlg3v76b627ar6mm8bgshjv51kag16swg5cc3k1rw1w3chm"))

(define rust-core-foundation-0.7.0
  (crate-source "core-foundation" "0.7.0"
                "0wbias8f0m5kyn2pcksi0h58fdslams6nmf16w78fgn42dx4rljp"))

(define rust-core-foundation-0.9.4
  (crate-source "core-foundation" "0.9.4"
                "13zvbbj07yk3b61b8fhwfzhy35535a583irf23vlcg59j7h9bqci"))

(define rust-core-foundation-sys-0.7.0
  (crate-source "core-foundation-sys" "0.7.0"
                "1b5qfnnmg49sawwfsb0c0wbj81bqi7h7lh68pmhbidf0jjs1m9xk"))

(define rust-core-foundation-sys-0.8.7
  (crate-source "core-foundation-sys" "0.8.7"
                "12w8j73lazxmr1z0h98hf3z623kl8ms7g07jch7n4p8f9nwlhdkp"))

(define rust-core-graphics-0.19.2
  (crate-source "core-graphics" "0.19.2"
                "08z9pgwfc0wb5v3ns7rnb2010q9g42b5vfwhp9fv4spawrs9725k"))

(define rust-core-graphics-0.21.0
  (crate-source "core-graphics" "0.21.0"
                "0y5rxchfhvjw25d19h9kz5pzshdngs26frgvx2n3w86gg11pr9jj"))

(define rust-coreaudio-rs-0.11.3
  (crate-source "coreaudio-rs" "0.11.3"
                "1kmssby4rqhv2iq1a8zmaav5p3bl40qs0wah9zv65ikr5lbpf41j"))

(define rust-coreaudio-sys-0.2.17
  (crate-source "coreaudio-sys" "0.2.17"
                "1dk4k2agjkn9ldhi9v5wjljqjjhzflx6zbrb3a9nybg6cxh7mv6f"))

(define rust-cpal-0.15.3
  (crate-source "cpal" "0.15.3"
                "0yd7d51kcf8ml0bfkjrac12zgfjzk21wa97maxg0fhzpr03sngc7"))

(define rust-crc32fast-1.5.0
  (crate-source "crc32fast" "1.5.0"
                "04d51liy8rbssra92p0qnwjw8i9rm9c4m3bwy19wjamz1k4w30cl"))

(define rust-dasp-sample-0.11.0
  (crate-source "dasp_sample" "0.11.0"
                "0zzw35akm3qs2rixbmlijk6h0l4g9ry6g74qc59zv1q8vs1f31qc"))

(define rust-deranged-0.5.6
  (crate-source "deranged" "0.5.6"
                "1i48p5l878bw4qzi1wz43lrq3jvplhpdzfxvjg0x3qn2janwagfc"))

(define rust-document-features-0.2.12
  (crate-source "document-features" "0.2.12"
                "0qcgpialq3zgvjmsvar9n6v10rfbv6mk6ajl46dd4pj5hn3aif6l"))

(define rust-either-1.15.0
  (crate-source "either" "1.15.0"
                "069p1fknsmzn9llaizh77kip0pqmcwpdsykv2x30xpjyija5gis8"))

(define rust-equivalent-1.0.2
  (crate-source "equivalent" "1.0.2"
                "03swzqznragy8n0x31lqc78g2af054jwivp7lkrbrc0khz74lyl7"))

(define rust-errno-0.3.14
  (crate-source "errno" "0.3.14"
                "1szgccmh8vgryqyadg8xd58mnwwicf39zmin3bsn63df2wbbgjir"))

(define rust-evdev-0.12.2
  (crate-source "evdev" "0.12.2"
                "19qh6r1z4v8ja6qqigjbg9vckbhlycc6wkqgzfz9fcln7almaq5b"))

(define rust-field-offset-0.3.6
  (crate-source "field-offset" "0.3.6"
                "0zq5sssaa2ckmcmxxbly8qgz3sxpb8g1lwv90sdh1z74qif2gqiq"))

(define rust-find-msvc-tools-0.1.9
  (crate-source "find-msvc-tools" "0.1.9"
                "10nmi0qdskq6l7zwxw5g56xny7hb624iki1c39d907qmfh3vrbjv"))

(define rust-flate2-1.1.9
  (crate-source "flate2" "1.1.9"
                "0g2pb7cxnzcbzrj8bw4v6gpqqp21aycmf6d84rzb6j748qkvlgw4"))

(define rust-foldhash-0.1.5
  (crate-source "foldhash" "0.1.5"
                "1wisr1xlc2bj7hk4rgkcjkz3j2x4dhd1h9lwk7mj8p71qpdgbi6r"))

(define rust-foreign-types-0.3.2
  (crate-source "foreign-types" "0.3.2"
                "1cgk0vyd7r45cj769jym4a6s7vwshvd0z4bqrb92q1fwibmkkwzn"))

(define rust-foreign-types-shared-0.1.1
  (crate-source "foreign-types-shared" "0.1.1"
                "0jxgzd04ra4imjv8jgkmdq59kj8fsz6w4zxsbmlai34h26225c00"))

(define rust-form-urlencoded-1.2.2
  (crate-source "form_urlencoded" "1.2.2"
                "1kqzb2qn608rxl3dws04zahcklpplkd5r1vpabwga5l50d2v4k6b"))

(define rust-funty-2.0.0
  (crate-source "funty" "2.0.0"
                "177w048bm0046qlzvp33ag3ghqkqw4ncpzcm5lq36gxf2lla7mg6"))

(define rust-futures-channel-0.3.32
  (crate-source "futures-channel" "0.3.32"
                "07fcyzrmbmh7fh4ainilf1s7gnwvnk07phdq77jkb9fpa2ffifq7"))

(define rust-futures-core-0.3.32
  (crate-source "futures-core" "0.3.32"
                "07bbvwjbm5g2i330nyr1kcvjapkmdqzl4r6mqv75ivvjaa0m0d3y"))

(define rust-futures-executor-0.3.32
  (crate-source "futures-executor" "0.3.32"
                "17aplz3ns74qn7a04qg7qlgsdx5iwwwkd4jvdfra6hl3h4w9rwms"))

(define rust-futures-io-0.3.32
  (crate-source "futures-io" "0.3.32"
                "063pf5m6vfmyxj74447x8kx9q8zj6m9daamj4hvf49yrg9fs7jyf"))

(define rust-futures-macro-0.3.32
  (crate-source "futures-macro" "0.3.32"
                "0ys4b1lk7s0bsj29pv42bxsaavalch35rprp64s964p40c1bfdg8"))

(define rust-futures-task-0.3.32
  (crate-source "futures-task" "0.3.32"
                "14s3vqf8llz3kjza33vn4ixg6kwxp61xrysn716h0cwwsnri2xq3"))

(define rust-futures-util-0.3.32
  (crate-source "futures-util" "0.3.32"
                "1mn60lw5kh32hz9isinjlpw34zx708fk5q1x0m40n6g6jq9a971q"))

(define rust-gdk-pixbuf-0.20.10
  (crate-source "gdk-pixbuf" "0.20.10"
                "0371cfhxldrn2pf8zdjyx2b3xkhbfm96k988spp4nkq89j4l5lig"))

(define rust-gdk-pixbuf-sys-0.20.10
  (crate-source "gdk-pixbuf-sys" "0.20.10"
                "15hb2f5mmyg5amaha6lx6spaygw2b7ga4hwmgqhvv269h2sz6d2v"))

(define rust-gdk4-0.9.6
  (crate-source "gdk4" "0.9.6"
                "0q1dld01fgj7qxj644by0fc242mcn36w3bagn4z1mkdfq7cwjl28"))

(define rust-gdk4-sys-0.9.6
  (crate-source "gdk4-sys" "0.9.6"
                "0fj722lp86fpa1b1i3s2anavdmcpybd0b47mkhknzd72k1bvjvkg"))

(define rust-getrandom-0.2.17
  (crate-source "getrandom" "0.2.17"
                "1l2ac6jfj9xhpjjgmcx6s1x89bbnw9x6j9258yy6xjkzpq0bqapz"))

(define rust-getrandom-0.3.4
  (crate-source "getrandom" "0.3.4"
                "1zbpvpicry9lrbjmkd4msgj3ihff1q92i334chk7pzf46xffz7c9"))

(define rust-getrandom-0.4.1
  (crate-source "getrandom" "0.4.1"
                "1v7fm84f2jh6x7w3bd2ncl3sw29wnb0rhg7xya1pd30i02cg77hk"))

(define rust-gio-0.20.12
  (crate-source "gio" "0.20.12"
                "0cdq5116cwdgs0xkdp1v146yhcqilxlpgvkncc7xbf5nwxvf49wf"))

(define rust-gio-sys-0.20.10
  (crate-source "gio-sys" "0.20.10"
                "10vc6gqhz5crnrh040rv6r5nm09njky2r9d9ms29xj3gwnkr67jj"))

(define rust-glib-0.20.12
  (crate-source "glib" "0.20.12"
                "10ynn8aiabbzrsgdswmqpr47sapfkbfn5rfxsy26swflabivdi7z"))

(define rust-glib-macros-0.20.12
  (crate-source "glib-macros" "0.20.12"
                "0ibi9vbpbw9vvl9ax60kxq07d7a21k0jj5lva8zmliq95zv4l278"))

(define rust-glib-sys-0.20.10
  (crate-source "glib-sys" "0.20.10"
                "05f29ky5dnvy8vp5rdld5f8r2lgr5w7dxqr7p27km016s4g9xdwa"))

(define rust-glob-0.3.3
  (crate-source "glob" "0.3.3"
                "106jpd3syfzjfj2k70mwm0v436qbx96wig98m4q8x071yrq35hhc"))

(define rust-gobject-sys-0.20.10
  (crate-source "gobject-sys" "0.20.10"
                "1niyqv22b2c38ks33i4isas4v83d3w7jx3xzzly9x63kpfacm6pc"))

(define rust-graphene-rs-0.20.10
  (crate-source "graphene-rs" "0.20.10"
                "16six67j0j57ynv7frxiwnsf7dslhyy67ppirad1q98lgnnxz1kb"))

(define rust-graphene-sys-0.20.10
  (crate-source "graphene-sys" "0.20.10"
                "1sk1736b4vay2hj9qz56c0pvqa3v0mkdch3yg7hiapidpa2kln6z"))

(define rust-gsk4-0.9.6
  (crate-source "gsk4" "0.9.6"
                "0mgqq5m6cm4q7ajjgw92z13z2ikpvh6zx2gwzdjrz30wjcpygxb1"))

(define rust-gsk4-sys-0.9.6
  (crate-source "gsk4-sys" "0.9.6"
                "1p1n4jhhxyvj7hb0cqhzvazrck0qw81sz36ydfj8avzsapg5jl3m"))

(define rust-gtk4-0.9.7
  (crate-source "gtk4" "0.9.7"
                "1mi6lcwm25jz7lznrb9glaabgyk40hnvkg4fzaxlf762080xsx7j"))

(define rust-gtk4-macros-0.9.5
  (crate-source "gtk4-macros" "0.9.5"
                "169rqfxfczivcpz7019slsrpkx8crqjka43ymxmikp838xn7il8f"))

(define rust-gtk4-sys-0.9.6
  (crate-source "gtk4-sys" "0.9.6"
                "1mh3xjkjb99y97z234cvyar08vcr7zblg1nrw48c6xsdwl0kpq21"))

(define rust-hashbrown-0.15.5
  (crate-source "hashbrown" "0.15.5"
                "189qaczmjxnikm9db748xyhiw04kpmhm9xj9k9hg0sgx7pjwyacj"))

(define rust-hashbrown-0.16.1
  (crate-source "hashbrown" "0.16.1"
                "004i3njw38ji3bzdp9z178ba9x3k0c1pgy8x69pj7yfppv4iq7c4"))

(define rust-hotkey-listener-0.3.2
  (crate-source "hotkey-listener" "0.3.2"
                "0mg55432ki82iqlm7f6v3nbgcgdp1clw05k6nzlai2d4hjzx69ck"))

(define rust-hound-3.5.1
  (crate-source "hound" "3.5.1"
                "0kw5yybfc7hdwxwm6d3m3h4ms52fkw0n0zch35drb52ci2xsmbb2"))

(define rust-http-1.4.0
  (crate-source "http" "1.4.0"
                "06iind4cwsj1d6q8c2xgq8i2wka4ps74kmws24gsi1bzdlw2mfp3"))

(define rust-httparse-1.10.1
  (crate-source "httparse" "1.10.1"
                "11ycd554bw2dkgw0q61xsa7a4jn1wb1xbfacmf3dbwsikvkkvgvd"))

(define rust-humantime-2.3.0
  (crate-source "humantime" "2.3.0"
                "092lpipp32ayz4kyyn4k3vz59j9blng36wprm5by0g2ykqr14nqk"))

(define rust-humantime-serde-1.1.1
  (crate-source "humantime-serde" "1.1.1"
                "0310ri4zb33qbwqv0n51xysfjpnwc6rgxscl5i09jgcjlmgdp8sp"))

(define rust-icu-collections-2.1.1
  (crate-source "icu_collections" "2.1.1"
                "0hsblchsdl64q21qwrs4hvc2672jrf466zivbj1bwyv606bn8ssc"))

(define rust-icu-locale-core-2.1.1
  (crate-source "icu_locale_core" "2.1.1"
                "1djvdc2f5ylmp1ymzv4gcnmq1s4hqfim9nxlcm173lsd01hpifpd"))

(define rust-icu-normalizer-2.1.1
  (crate-source "icu_normalizer" "2.1.1"
                "16dmn5596la2qm0r3vih0bzjfi0vx9a20yqjha6r1y3vnql8hv2z"))

(define rust-icu-normalizer-data-2.1.1
  (crate-source "icu_normalizer_data" "2.1.1"
                "02jnzizg6q75m41l6c13xc7nkc5q8yr1b728dcgfhpzw076wrvbs"))

(define rust-icu-properties-2.1.2
  (crate-source "icu_properties" "2.1.2"
                "1v3lbmhhi7i6jgw51ikjb1p50qh5rb67grlkdnkc63l7zq1gq2q2"))

(define rust-icu-properties-data-2.1.2
  (crate-source "icu_properties_data" "2.1.2"
                "1bvpkh939rgzrjfdb7hz47v4wijngk0snmcgrnpwc9fpz162jv31"))

(define rust-icu-provider-2.1.1
  (crate-source "icu_provider" "2.1.1"
                "0576b7dizgyhpfa74kacv86y4g1p7v5ffd6c56kf1q82rvq2r5l5"))

(define rust-id-arena-2.3.0
  (crate-source "id-arena" "2.3.0"
                "0m6rs0jcaj4mg33gkv98d71w3hridghp5c4yr928hplpkgbnfc1x"))

(define rust-idna-1.1.0
  (crate-source "idna" "1.1.0"
                "1pp4n7hppm480zcx411dsv9wfibai00wbpgnjj4qj0xa7kr7a21v"))

(define rust-idna-adapter-1.2.1
  (crate-source "idna_adapter" "1.2.1"
                "0i0339pxig6mv786nkqcxnwqa87v4m94b2653f6k3aj0jmhfkjis"))

(define rust-indexmap-2.13.0
  (crate-source "indexmap" "2.13.0"
                "05qh5c4h2hrnyypphxpwflk45syqbzvqsvvyxg43mp576w2ff53p"))

(define rust-is-terminal-polyfill-1.70.2
  (crate-source "is_terminal_polyfill" "1.70.2"
                "15anlc47sbz0jfs9q8fhwf0h3vs2w4imc030shdnq54sny5i7jx6"))

(define rust-itertools-0.13.0
  (crate-source "itertools" "0.13.0"
                "11hiy3qzl643zcigknclh446qb9zlg4dpdzfkjaa9q9fqpgyfgj1"))

(define rust-itoa-1.0.17
  (crate-source "itoa" "1.0.17"
                "1lh93xydrdn1g9x547bd05g0d3hra7pd1k4jfd2z1pl1h5hwdv4j"))

(define rust-jni-0.21.1
  (crate-source "jni" "0.21.1"
                "15wczfkr2r45slsljby12ymf2hij8wi5b104ghck9byjnwmsm1qs"))

(define rust-jni-sys-0.3.0
  (crate-source "jni-sys" "0.3.0"
                "0c01zb9ygvwg9wdx2fii2d39myzprnpqqhy7yizxvjqp5p04pbwf"))

(define rust-jobserver-0.1.34
  (crate-source "jobserver" "0.1.34"
                "0cwx0fllqzdycqn4d6nb277qx5qwnmjdxdl0lxkkwssx77j3vyws"))

(define rust-js-sys-0.3.85
  (crate-source "js-sys" "0.3.85"
                "1csmb42fxjmzjdgc790bgw77sf1cb9ydm5rdsnh5qj4miszjx54c"))

(define rust-leb128fmt-0.1.0
  (crate-source "leb128fmt" "0.1.0"
                "1chxm1484a0bly6anh6bd7a99sn355ymlagnwj3yajafnpldkv89"))

(define rust-libc-0.2.182
  (crate-source "libc" "0.2.182"
                "04k1w1mq9f4cxv520dbr5xw1i7xkbc9fcrvaggyjy25jdkdvl038"))

(define rust-libloading-0.8.9
  (crate-source "libloading" "0.8.9"
                "0mfwxwjwi2cf0plxcd685yxzavlslz7xirss3b9cbrzyk4hv1i6p"))

(define rust-linux-raw-sys-0.11.0
  (crate-source "linux-raw-sys" "0.11.0"
                "0fghx0nn8nvbz5yzgizfcwd6ap2pislp68j8c1bwyr6sacxkq7fz"))

(define rust-litemap-0.8.1
  (crate-source "litemap" "0.8.1"
                "0xsy8pfp9s802rsj1bq2ys2kbk1g36w5dr3gkfip7gphb5x60wv3"))

(define rust-litrs-1.0.0
  (crate-source "litrs" "1.0.0"
                "14p0kzzkavnngvybl88nvfwv031cc2qx4vaxpfwsiifm8grdglqi"))

(define rust-log-0.4.29
  (crate-source "log" "0.4.29"
                "15q8j9c8g5zpkcw0hnd6cf2z7fxqnvsjh3rw5mv5q10r83i34l2y"))

(define rust-mach2-0.4.3
  (crate-source "mach2" "0.4.3"
                "0i6vcnbq5v51whgyidzhf7cbxqrmj2nkw8z0m2ib02rc60mjhh6n"))

(define rust-malloc-buf-0.0.6
  (crate-source "malloc_buf" "0.0.6"
                "1jqr77j89pwszv51fmnknzvd53i1nkmcr8rjrvcxhm4dx1zr1fv2"))

(define rust-matchers-0.2.0
  (crate-source "matchers" "0.2.0"
                "1sasssspdj2vwcwmbq3ra18d3qniapkimfcbr47zmx6750m5llni"))

(define rust-memchr-2.8.0
  (crate-source "memchr" "2.8.0"
                "0y9zzxcqxvdqg6wyag7vc3h0blhdn7hkq164bxyx2vph8zs5ijpq"))

(define rust-memoffset-0.6.5
  (crate-source "memoffset" "0.6.5"
                "1kkrzll58a3ayn5zdyy9i1f1v3mx0xgl29x0chq614zazba638ss"))

(define rust-memoffset-0.9.1
  (crate-source "memoffset" "0.9.1"
                "12i17wh9a9plx869g7j4whf62xw68k5zd4k0k5nh6ys5mszid028"))

(define rust-miniz-oxide-0.8.9
  (crate-source "miniz_oxide" "0.8.9"
                "05k3pdg8bjjzayq3rf0qhpirq9k37pxnasfn4arbs17phqn6m9qz"))

(define rust-ndk-0.8.0
  (crate-source "ndk" "0.8.0"
                "1dx5yyqh32bi161mipg4br4i33syjidw81qrq0w7mc8hf0ds6xi0"))

(define rust-ndk-context-0.1.1
  (crate-source "ndk-context" "0.1.1"
                "12sai3dqsblsvfd1l1zab0z6xsnlha3xsfl7kagdnmj3an3jvc17"))

(define rust-nix-0.23.2
  (crate-source "nix" "0.23.2"
                "0p5kxhm5d8lry0szqbsllpcb5i3z7lg1dkglw0ni2l011b090dwg"))

(define rust-nu-ansi-term-0.50.3
  (crate-source "nu-ansi-term" "0.50.3"
                "1ra088d885lbd21q1bxgpqdlk1zlndblmarn948jz2a40xsbjmvr"))

(define rust-num-conv-0.2.0
  (crate-source "num-conv" "0.2.0"
                "0l4hj7lp8zbb9am4j3p7vlcv47y9bbazinvnxx9zjhiwkibyr5yg"))

(define rust-num-derive-0.4.2
  (crate-source "num-derive" "0.4.2"
                "00p2am9ma8jgd2v6xpsz621wc7wbn1yqi71g15gc3h67m7qmafgd"))

(define rust-num-enum-0.7.5
  (crate-source "num_enum" "0.7.5"
                "0k25hagf3xfgmj4j1zmvja1d6844jrmpginxpd3vhmxd41z7l85i"))

(define rust-num-enum-derive-0.7.5
  (crate-source "num_enum_derive" "0.7.5"
                "1mx4dgza8b9g16baybc00gg06jn4cf17h45p0fr3qx5nw5fkccpz"))

(define rust-objc-0.2.7
  (crate-source "objc" "0.2.7"
                "1cbpf6kz8a244nn1qzl3xyhmp05gsg4n313c9m3567625d3innwi"))

(define rust-oboe-0.6.1
  (crate-source "oboe" "0.6.1"
                "1yv7x06mwk61nsy3ckcmqwgg9q0n3j4y4zncz3sl6pcyskmipdp8"))

(define rust-oboe-sys-0.6.1
  (crate-source "oboe-sys" "0.6.1"
                "17g7yb4kk6bakc4rhv1izfcqjgqhpkasgq6gf20nc79b9adb12vc"))

(define rust-once-cell-1.21.3
  (crate-source "once_cell" "1.21.3"
                "0b9x77lb9f1j6nqgf5aka4s2qj0nly176bpbrv6f9iakk5ff3xa2"))

(define rust-once-cell-polyfill-1.70.2
  (crate-source "once_cell_polyfill" "1.70.2"
                "1zmla628f0sk3fhjdjqzgxhalr2xrfna958s632z65bjsfv8ljrq"))

(define rust-pango-0.20.12
  (crate-source "pango" "0.20.12"
                "0p5bj7k8sd2pgm7v907i9bip53ys46hahprs0jbr6rfzyq8v6xk5"))

(define rust-pango-sys-0.20.10
  (crate-source "pango-sys" "0.20.10"
                "1yj3n87whqx6gw3vip08zbckqxfg7l5jqc2wamaf76y07xkhjs8q"))

(define rust-percent-encoding-2.3.2
  (crate-source "percent-encoding" "2.3.2"
                "083jv1ai930azvawz2khv7w73xh8mnylk7i578cifndjn5y64kwv"))

(define rust-pin-project-lite-0.2.16
  (crate-source "pin-project-lite" "0.2.16"
                "16wzc7z7dfkf9bmjin22f5282783f6mdksnr0nv0j5ym5f9gyg1v"))

(define rust-pkg-config-0.3.32
  (crate-source "pkg-config" "0.3.32"
                "0k4h3gnzs94sjb2ix6jyksacs52cf1fanpwsmlhjnwrdnp8dppby"))

(define rust-potential-utf-0.1.4
  (crate-source "potential_utf" "0.1.4"
                "0xxg0pkfpq299wvwln409z4fk80rbv55phh3f1jhjajy5x1ljfdp"))

(define rust-ppv-lite86-0.2.21
  (crate-source "ppv-lite86" "0.2.21"
                "1abxx6qz5qnd43br1dd9b2savpihzjza8gb4fbzdql1gxp2f7sl5"))

(define rust-prettyplease-0.2.37
  (crate-source "prettyplease" "0.2.37"
                "0azn11i1kh0byabhsgab6kqs74zyrg69xkirzgqyhz6xmjnsi727"))

(define rust-proc-macro-crate-3.4.0
  (crate-source "proc-macro-crate" "3.4.0"
                "10v9qi51n4phn1lrj5r94kjq7yhci9jrkqnn6wpan05yjsgb3711"))

(define rust-proc-macro2-1.0.106
  (crate-source "proc-macro2" "1.0.106"
                "0d09nczyaj67x4ihqr5p7gxbkz38gxhk4asc0k8q23g9n85hzl4g"))

(define rust-proptest-1.10.0
  (crate-source "proptest" "1.10.0"
                "0ch5r381al5z7089j47gkyybzbgygkgld5bzfg019vxcznrnqmip"))

(define rust-quote-1.0.44
  (crate-source "quote" "1.0.44"
                "1r7c7hxl66vz3q9qizgjhy77pdrrypqgk4ghc7260xvvfb7ypci1"))

(define rust-r-efi-5.3.0
  (crate-source "r-efi" "5.3.0"
                "03sbfm3g7myvzyylff6qaxk4z6fy76yv860yy66jiswc2m6b7kb9"))

(define rust-radium-0.7.0
  (crate-source "radium" "0.7.0"
                "02cxfi3ky3c4yhyqx9axqwhyaca804ws46nn4gc1imbk94nzycyw"))

(define rust-rand-0.9.2
  (crate-source "rand" "0.9.2"
                "1lah73ainvrgl7brcxx0pwhpnqa3sm3qaj672034jz8i0q7pgckd"))

(define rust-rand-chacha-0.9.0
  (crate-source "rand_chacha" "0.9.0"
                "1jr5ygix7r60pz0s1cv3ms1f6pd1i9pcdmnxzzhjc3zn3mgjn0nk"))

(define rust-rand-core-0.9.5
  (crate-source "rand_core" "0.9.5"
                "0g6qc5r3f0hdmz9b11nripyp9qqrzb0xqk9piip8w8qlvqkcibvn"))

(define rust-rand-xorshift-0.4.0
  (crate-source "rand_xorshift" "0.4.0"
                "0njsn25pis742gb6b89cpq7jp48v9n23a9fvks10yczwks8n4fai"))

(define rust-rdev-0.5.3
  (crate-source "rdev" "0.5.3"
                "1hv0wd5xr4yb3dlir7nq8571wha9jpj1sn5msx6bi4rgvji2qm80"))

(define rust-regex-1.12.3
  (crate-source "regex" "1.12.3"
                "0xp2q0x7ybmpa5zlgaz00p8zswcirj9h8nry3rxxsdwi9fhm81z1"))

(define rust-regex-automata-0.4.14
  (crate-source "regex-automata" "0.4.14"
                "13xf7hhn4qmgfh784llcp2kzrvljd13lb2b1ca0mwnf15w9d87bf"))

(define rust-regex-syntax-0.8.9
  (crate-source "regex-syntax" "0.8.9"
                "0k0a47r1rcl794wj8a948niakbg081s5pp5nlgcbmmr2iy3qfs59"))

(define rust-ring-0.17.14
  (crate-source "ring" "0.17.14"
                "1dw32gv19ccq4hsx3ribhpdzri1vnrlcfqb2vj41xn4l49n9ws54"))

(define rust-rustc-hash-2.1.1
  (crate-source "rustc-hash" "2.1.1"
                "03gz5lvd9ghcwsal022cgkq67dmimcgdjghfb5yb5d352ga06xrm"))

(define rust-rustc-version-0.4.1
  (crate-source "rustc_version" "0.4.1"
                "14lvdsmr5si5qbqzrajgb6vfn69k0sfygrvfvr2mps26xwi3mjyg"))

(define rust-rustix-1.1.3
  (crate-source "rustix" "1.1.3"
                "0d0z2zcw4rwzni1hm8snw8xdxwwrij336m31c4ghq66cghj9wv0l"))

(define rust-rustls-0.23.36
  (crate-source "rustls" "0.23.36"
                "06w0077ssk3blpp93613lkny046mwj0nhxjgc7cmg9nf70yz6rf6"))

(define rust-rustls-pki-types-1.14.0
  (crate-source "rustls-pki-types" "1.14.0"
                "1p9zsgslvwzzkzhm6bqicffqndr4jpx67992b0vl0pi21a5hy15y"))

(define rust-rustls-webpki-0.103.9
  (crate-source "rustls-webpki" "0.103.9"
                "0lwg1nnyv7pp2lfwwjhy81bxm233am99jnsp3iymdhd6k8827pyp"))

(define rust-rustversion-1.0.22
  (crate-source "rustversion" "1.0.22"
                "0vfl70jhv72scd9rfqgr2n11m5i9l1acnk684m2w83w0zbqdx75k"))

(define rust-rusty-fork-0.3.1
  (crate-source "rusty-fork" "0.3.1"
                "1qkf9rvz2irb1wlbkrhrns8n9hnax48z1lgql5nqyr2fyagzfsyc"))

(define rust-ryu-1.0.23
  (crate-source "ryu" "1.0.23"
                "0zs70sg00l2fb9jwrf6cbkdyscjs53anrvai2hf7npyyfi5blx4p"))

(define rust-secstr-0.5.1
  (crate-source "secstr" "0.5.1"
                "04iy25y5qb8lzymsx1iz6250q1dxx29mkppn737w81gn8ir6akz0"))

(define rust-semver-1.0.27
  (crate-source "semver" "1.0.27"
                "1qmi3akfrnqc2hfkdgcxhld5bv961wbk8my3ascv5068mc5fnryp"))

(define rust-serde-1.0.228
  (crate-source "serde" "1.0.228"
                "17mf4hhjxv5m90g42wmlbc61hdhlm6j9hwfkpcnd72rpgzm993ls"))

(define rust-serde-core-1.0.228
  (crate-source "serde_core" "1.0.228"
                "1bb7id2xwx8izq50098s5j2sqrrvk31jbbrjqygyan6ask3qbls1"))

(define rust-serde-derive-1.0.228
  (crate-source "serde_derive" "1.0.228"
                "0y8xm7fvmr2kjcd029g9fijpndh8csv5m20g4bd76w8qschg4h6m"))

(define rust-serde-json-1.0.149
  (crate-source "serde_json" "1.0.149"
                "11jdx4vilzrjjd1dpgy67x5lgzr0laplz30dhv75lnf5ffa07z43"))

(define rust-serde-spanned-1.0.4
  (crate-source "serde_spanned" "1.0.4"
                "0xkp0qdzams5sqwndbw3xrhf4c0bb5r46w2ywkp1aqsdb8ggkfzq"))

(define rust-sharded-slab-0.1.7
  (crate-source "sharded-slab" "0.1.7"
                "1xipjr4nqsgw34k7a2cgj9zaasl2ds6jwn89886kww93d32a637l"))

(define rust-simd-adler32-0.3.8
  (crate-source "simd-adler32" "0.3.8"
                "18lx2gdgislabbvlgw5q3j5ssrr77v8kmkrxaanp3liimp2sc873"))

(define rust-slab-0.4.12
  (crate-source "slab" "0.4.12"
                "1xcwik6s6zbd3lf51kkrcicdq2j4c1fw0yjdai2apy9467i0sy8c"))

(define rust-smallvec-1.15.1
  (crate-source "smallvec" "1.15.1"
                "00xxdxxpgyq5vjnpljvkmy99xij5rxgh913ii1v16kzynnivgcb7"))

(define rust-stable-deref-trait-1.2.1
  (crate-source "stable_deref_trait" "1.2.1"
                "15h5h73ppqyhdhx6ywxfj88azmrpml9gl6zp3pwy2malqa6vxqkc"))

(define rust-subtle-2.6.1
  (crate-source "subtle" "2.6.1"
                "14ijxaymghbl1p0wql9cib5zlwiina7kall6w7g89csprkgbvhhk"))

(define rust-syn-2.0.117
  (crate-source "syn" "2.0.117"
                "16cv7c0wbn8amxc54n4w15kxlx5ypdmla8s0gxr2l7bv7s0bhrg6"))

(define rust-synstructure-0.13.2
  (crate-source "synstructure" "0.13.2"
                "1lh9lx3r3jb18f8sbj29am5hm9jymvbwh6jb1izsnnxgvgrp12kj"))

(define rust-system-deps-7.0.7
  (crate-source "system-deps" "7.0.7"
                "0zsyh2m893nqkp1wri5c85favp2xyl1qpjxnd5nz31pr6qvz7j28"))

(define rust-tap-1.0.1
  (crate-source "tap" "1.0.1"
                "0sc3gl4nldqpvyhqi3bbd0l9k7fngrcl4zs47n314nqqk4bpx4sm"))

(define rust-target-lexicon-0.13.3
  (crate-source "target-lexicon" "0.13.3"
                "0355pbycq0cj29h1rp176l57qnfwmygv7hwzchs7iq15gibn4zyz"))

(define rust-tempfile-3.25.0
  (crate-source "tempfile" "3.25.0"
                "1wg5jnzbgpb1wmw396v31f0c70dvj5mpik7rk7fzdccmghgpjdh1"))

(define rust-thiserror-2.0.18
  (crate-source "thiserror" "2.0.18"
                "1i7vcmw9900bvsmay7mww04ahahab7wmr8s925xc083rpjybb222"))

(define rust-thiserror-impl-2.0.18
  (crate-source "thiserror-impl" "2.0.18"
                "1mf1vrbbimj1g6dvhdgzjmn6q09yflz2b92zs1j9n3k7cxzyxi7b"))

(define rust-thread-local-1.1.9
  (crate-source "thread_local" "1.1.9"
                "1191jvl8d63agnq06pcnarivf63qzgpws5xa33hgc92gjjj4c0pn"))

(define rust-time-0.3.47
  (crate-source "time" "0.3.47"
                "0b7g9ly2iabrlgizliz6v5x23yq5d6bpp0mqz6407z1s526d8fvl"))

(define rust-time-core-0.1.8
  (crate-source "time-core" "0.1.8"
                "1jidl426mw48i7hjj4hs9vxgd9lwqq4vyalm4q8d7y4iwz7y353n"))

(define rust-time-macros-0.2.27
  (crate-source "time-macros" "0.2.27"
                "058ja265waq275wxvnfwavbz9r1hd4dgwpfn7a1a9a70l32y8w1f"))

(define rust-tinystr-0.8.2
  (crate-source "tinystr" "0.8.2"
                "0sa8z88axdsf088hgw5p4xcyi6g3w3sgbb6qdp81bph9bk2fkls2"))

(define rust-tracing-0.1.44
  (crate-source "tracing" "0.1.44"
                "006ilqkg1lmfdh3xhg3z762izfwmxcvz0w7m4qx2qajbz9i1drv3"))

(define rust-tracing-attributes-0.1.31
  (crate-source "tracing-attributes" "0.1.31"
                "1np8d77shfvz0n7camx2bsf1qw0zg331lra0hxb4cdwnxjjwz43l"))

(define rust-tracing-core-0.1.36
  (crate-source "tracing-core" "0.1.36"
                "16mpbz6p8vd6j7sf925k9k8wzvm9vdfsjbynbmaxxyq6v7wwm5yv"))

(define rust-tracing-log-0.2.0
  (crate-source "tracing-log" "0.2.0"
                "1hs77z026k730ij1a9dhahzrl0s073gfa2hm5p0fbl0b80gmz1gf"))

(define rust-tracing-subscriber-0.3.22
  (crate-source "tracing-subscriber" "0.3.22"
                "07hz575a0p1c2i4xw3gs3hkrykhndnkbfhyqdwjhvayx4ww18c1g"))

(define rust-unarray-0.1.4
  (crate-source "unarray" "0.1.4"
                "154smf048k84prsdgh09nkm2n0w0336v84jd4zikyn6v6jrqbspa"))

(define rust-unicode-ident-1.0.24
  (crate-source "unicode-ident" "1.0.24"
                "0xfs8y1g7syl2iykji8zk5hgfi5jw819f5zsrbaxmlzwsly33r76"))

(define rust-unicode-xid-0.2.6
  (crate-source "unicode-xid" "0.2.6"
                "0lzqaky89fq0bcrh6jj6bhlz37scfd8c7dsj5dq7y32if56c1hgb"))

(define rust-untrusted-0.9.0
  (crate-source "untrusted" "0.9.0"
                "1ha7ib98vkc538x0z60gfn0fc5whqdd85mb87dvisdcaifi6vjwf"))

(define rust-ureq-3.2.0
  (crate-source "ureq" "3.2.0"
                "1z11gb6bmaq3h0qncrknmkc1pqdkck6d7rx7ybnwz1avawl7mjgx"))

(define rust-ureq-proto-0.5.3
  (crate-source "ureq-proto" "0.5.3"
                "0vzdcxabp5qs1b5mhsjb94mh82m12n40csm46icvwcphkpx9w7yq"))

(define rust-url-2.5.8
  (crate-source "url" "2.5.8"
                "1v8f7nx3hpr1qh76if0a04sj08k86amsq4h8cvpw6wvk76jahrzz"))

(define rust-utf-8-0.7.6
  (crate-source "utf-8" "0.7.6"
                "1a9ns3fvgird0snjkd3wbdhwd3zdpc2h5gpyybrfr6ra5pkqxk09"))

(define rust-valuable-0.1.1
  (crate-source "valuable" "0.1.1"
                "0r9srp55v7g27s5bg7a2m095fzckrcdca5maih6dy9bay6fflwxs"))

(define rust-version-compare-0.2.1
  (crate-source "version-compare" "0.2.1"
                "03nziqxwnxlizl42cwsx33vi5xd2cf2jnszhh9rzay7g6xl8bhh3"))

(define rust-wait-timeout-0.2.1
  (crate-source "wait-timeout" "0.2.1"
                "04azqv9mnfxgvnc8j2wp362xraybakh2dy1nj22gj51rdl93pb09"))

(define rust-wasm-bindgen-0.2.108
  (crate-source "wasm-bindgen" "0.2.108"
                "0rl5pn80sdhj2p2r28lp3k50a8mpppzgwzssz2f3jdqyxhq4l0k4"))

(define rust-wasm-bindgen-futures-0.4.58
  (crate-source "wasm-bindgen-futures" "0.4.58"
                "0vqywn9df5i6mms3sw47v3kj7rzx8ryghqq0xb4jk05fs1zyg9kh"))

(define rust-wasm-bindgen-macro-0.2.108
  (crate-source "wasm-bindgen-macro" "0.2.108"
                "026nnvakp0w6j3ghpcxn31shj9wx8bv8x7nk3gkk40klkjfj72q0"))

(define rust-wasm-bindgen-macro-support-0.2.108
  (crate-source "wasm-bindgen-macro-support" "0.2.108"
                "0m9sj475ypgifbkvksjsqs2gy3bq96f87ychch784m4gspiblmjj"))

(define rust-wasm-bindgen-shared-0.2.108
  (crate-source "wasm-bindgen-shared" "0.2.108"
                "04ix7v99rvj5730553j58pqsrwpf9sqazr60y3cchx5cr60ba08z"))

(define rust-wasm-encoder-0.244.0
  (crate-source "wasm-encoder" "0.244.0"
                "06c35kv4h42vk3k51xjz1x6hn3mqwfswycmr6ziky033zvr6a04r"))

(define rust-wasm-metadata-0.244.0
  (crate-source "wasm-metadata" "0.244.0"
                "02f9dhlnryd2l7zf03whlxai5sv26x4spfibjdvc3g9gd8z3a3mv"))

(define rust-wasmparser-0.244.0
  (crate-source "wasmparser" "0.244.0"
                "1zi821hrlsxfhn39nqpmgzc0wk7ax3dv6vrs5cw6kb0v5v3hgf27"))

(define rust-web-sys-0.3.85
  (crate-source "web-sys" "0.3.85"
                "1645c202gyw21m6kxw4ya81vrapl40hlb8m9iqhjj8fra7jk4bii"))

(define rust-webpki-roots-1.0.6
  (crate-source "webpki-roots" "1.0.6"
                "1v8brkarm4spqkjs6y5b67xixnz4zlg33d1wwxigz4rr0qyazkr2"))

(define rust-winapi-util-0.1.11
  (crate-source "winapi-util" "0.1.11"
                "08hdl7mkll7pz8whg869h58c1r9y7in0w0pk8fm24qc77k0b39y2"))

(define rust-windows-0.54.0
  (crate-source "windows" "0.54.0"
                "0j8vd8sg2rbln6g3a608qg1a7r2lwxcga78mmxjjin5ybmrfallj"))

(define rust-windows-core-0.54.0
  (crate-source "windows-core" "0.54.0"
                "0r8x2sgl4qq1h23ldf4z7cj213k0bz7479m8a156h79mi6f1nrhj"))

(define rust-windows-link-0.2.1
  (crate-source "windows-link" "0.2.1"
                "1rag186yfr3xx7piv5rg8b6im2dwcf8zldiflvb22xbzwli5507h"))

(define rust-windows-result-0.1.2
  (crate-source "windows-result" "0.1.2"
                "1y274q1v0vy21lhkgslpxpq1m08hvr1mcs2l88h1b1gcx0136f2y"))

(define rust-windows-sys-0.45.0
  (crate-source "windows-sys" "0.45.0"
                "1l36bcqm4g89pknfp8r9rl1w4bn017q6a8qlx8viv0xjxzjkna3m"))

(define rust-windows-sys-0.61.2
  (crate-source "windows-sys" "0.61.2"
                "1z7k3y9b6b5h52kid57lvmvm05362zv1v8w0gc7xyv5xphlp44xf"))

(define rust-windows-targets-0.42.2
  (crate-source "windows-targets" "0.42.2"
                "0wfhnib2fisxlx8c507dbmh97kgij4r6kcxdi0f9nk6l1k080lcf"))

(define rust-windows-aarch64-gnullvm-0.42.2
  (crate-source "windows_aarch64_gnullvm" "0.42.2"
                "1y4q0qmvl0lvp7syxvfykafvmwal5hrjb4fmv04bqs0bawc52yjr"))

(define rust-windows-aarch64-msvc-0.42.2
  (crate-source "windows_aarch64_msvc" "0.42.2"
                "0hsdikjl5sa1fva5qskpwlxzpc5q9l909fpl1w6yy1hglrj8i3p0"))

(define rust-windows-i686-gnu-0.42.2
  (crate-source "windows_i686_gnu" "0.42.2"
                "0kx866dfrby88lqs9v1vgmrkk1z6af9lhaghh5maj7d4imyr47f6"))

(define rust-windows-i686-msvc-0.42.2
  (crate-source "windows_i686_msvc" "0.42.2"
                "0q0h9m2aq1pygc199pa5jgc952qhcnf0zn688454i7v4xjv41n24"))

(define rust-windows-x86-64-gnu-0.42.2
  (crate-source "windows_x86_64_gnu" "0.42.2"
                "0dnbf2xnp3xrvy8v9mgs3var4zq9v9yh9kv79035rdgyp2w15scd"))

(define rust-windows-x86-64-gnullvm-0.42.2
  (crate-source "windows_x86_64_gnullvm" "0.42.2"
                "18wl9r8qbsl475j39zvawlidp1bsbinliwfymr43fibdld31pm16"))

(define rust-windows-x86-64-msvc-0.42.2
  (crate-source "windows_x86_64_msvc" "0.42.2"
                "1w5r0q0yzx827d10dpjza2ww0j8iajqhmb54s735hhaj66imvv4s"))

(define rust-winnow-0.7.14
  (crate-source "winnow" "0.7.14"
                "0a88ahjqhyn2ln1yplq2xsigm09kxqkdkkk2c2mfxkbzszln8lss"))

(define rust-wit-bindgen-0.51.0
  (crate-source "wit-bindgen" "0.51.0"
                "19fazgch8sq5cvjv3ynhhfh5d5x08jq2pkw8jfb05vbcyqcr496p"))

(define rust-wit-bindgen-core-0.51.0
  (crate-source "wit-bindgen-core" "0.51.0"
                "1p2jszqsqbx8k7y8nwvxg65wqzxjm048ba5phaq8r9iy9ildwqga"))

(define rust-wit-bindgen-rust-0.51.0
  (crate-source "wit-bindgen-rust" "0.51.0"
                "08bzn5fsvkb9x9wyvyx98qglknj2075xk1n7c5jxv15jykh6didp"))

(define rust-wit-bindgen-rust-macro-0.51.0
  (crate-source "wit-bindgen-rust-macro" "0.51.0"
                "0ymizapzv2id89igxsz2n587y2hlfypf6n8kyp68x976fzyrn3qc"))

(define rust-wit-component-0.244.0
  (crate-source "wit-component" "0.244.0"
                "1clwxgsgdns3zj2fqnrjcp8y5gazwfa1k0sy5cbk0fsmx4hflrlx"))

(define rust-wit-parser-0.244.0
  (crate-source "wit-parser" "0.244.0"
                "0dm7avvdxryxd5b02l0g5h6933z1cw5z0d4wynvq2cywq55srj7c"))

(define rust-writeable-0.6.2
  (crate-source "writeable" "0.6.2"
                "1fg08y97n6vk7l0rnjggw3xyrii6dcqg54wqaxldrlk98zdy1pcy"))

(define rust-wyz-0.5.1
  (crate-source "wyz" "0.5.1"
                "1vdrfy7i2bznnzjdl9vvrzljvs4s3qm8bnlgqwln6a941gy61wq5"))

(define rust-x11-2.21.0
  (crate-source "x11" "2.21.0"
                "0bnvl09d7044k067gqdx1ln2r0ljp5f4675icwb0216d9i3aabah"))

(define rust-yoke-0.8.1
  (crate-source "yoke" "0.8.1"
                "0m29dm0bf5iakxgma0bj6dbmc3b8qi9b1vaw9sa76kdqmz3fbmkj"))

(define rust-yoke-derive-0.8.1
  (crate-source "yoke-derive" "0.8.1"
                "0pbyja133jnng4mrhimzdq4a0y26421g734ybgz8wsgbfhl0andn"))

(define rust-zerocopy-0.8.39
  (crate-source "zerocopy" "0.8.39"
                "0jmf1iqns5sq07k3dscsgyc706pycar67rrq4j9nrnzacgb3avfv"))

(define rust-zerocopy-derive-0.8.39
  (crate-source "zerocopy-derive" "0.8.39"
                "05z5yfq0mx3xdqadrgq5sd4d03nl82d9r0vp1qchaip9d4qws8j1"))

(define rust-zerofrom-0.1.6
  (crate-source "zerofrom" "0.1.6"
                "19dyky67zkjichsb7ykhv0aqws3q0jfvzww76l66c19y6gh45k2h"))

(define rust-zerofrom-derive-0.1.6
  (crate-source "zerofrom-derive" "0.1.6"
                "00l5niw7c1b0lf1vhvajpjmcnbdp2vn96jg4nmkhq2db0rp5s7np"))

(define rust-zeroize-1.8.2
  (crate-source "zeroize" "1.8.2"
                "1l48zxgcv34d7kjskr610zqsm6j2b4fcr2vfh9jm9j1jgvk58wdr"))

(define rust-zerotrie-0.2.3
  (crate-source "zerotrie" "0.2.3"
                "0lbqznlqazmrwwzslw0ci7p3pqxykrbfhq29npj0gmb2amxc2n9a"))

(define rust-zerovec-0.11.5
  (crate-source "zerovec" "0.11.5"
                "00m0p47k2g9mkv505hky5xh3r6ps7v8qc0dy4pspg542jj972a3c"))

(define rust-zerovec-derive-0.11.2
  (crate-source "zerovec-derive" "0.11.2"
                "1wsig4h5j7a1scd5hrlnragnazjny9qjc44hancb6p6a76ay7p7a"))

(define rust-zmij-1.0.21
  (crate-source "zmij" "1.0.21"
                "1amb5i6gz7yjb0dnmz5y669674pqmwbj44p4yfxfv2ncgvk8x15q"))

(define rust-ndk-sys-0.5.0+25.2.9519653
  (crate-source "ndk-sys" "0.5.0+25.2.9519653"
                "14bnxww0f17xl8pyn6j5kpkl98snjl9lin8i7qv4zzb0vmlnf6cc"))

(define rust-toml-0.9.12+spec-1.1.0
  (crate-source "toml" "0.9.12+spec-1.1.0"
                "0qwqbrymqn88mg2yqyq3rj52z6p20448z0jxdbpjsbpwg5g894ng"))

(define rust-toml-datetime-0.7.5+spec-1.1.0
  (crate-source "toml_datetime" "0.7.5+spec-1.1.0"
                "0iqkgvgsxmszpai53dbip7sf2igic39s4dby29dbqf1h9bnwzqcj"))

(define rust-toml-edit-0.23.10+spec-1.0.0
  (crate-source "toml_edit" "0.23.10+spec-1.0.0"
                "0saj5c676j8a3sqaj9akkp09wambg8aflji4zblwwa70azvvkj44"))

(define rust-toml-parser-1.0.9+spec-1.1.0
  (crate-source "toml_parser" "1.0.9+spec-1.1.0"
                "1i54qpvvcppy8ybdn9gssas81vfzq0kmgkcnxzhyf8w9w0al8bbh"))

(define rust-toml-writer-1.0.6+spec-1.1.0
  (crate-source "toml_writer" "1.0.6+spec-1.1.0"
                "01r6x42d1p8p5kzfsi1fm4dakm3w53vi69f2ivyqpvi1xm5g25mb"))

(define rust-wasi-0.11.1+wasi-snapshot-preview1
  (crate-source "wasi" "0.11.1+wasi-snapshot-preview1"
                "0jx49r7nbkbhyfrfyhz0bm4817yrnxgd3jiwwwfv0zl439jyrwyc"))

(define rust-wasip2-1.0.2+wasi-0.2.9
  (crate-source "wasip2" "1.0.2+wasi-0.2.9"
                "1xdw7v08jpfjdg94sp4lbdgzwa587m5ifpz6fpdnkh02kwizj5wm"))

(define rust-wasip3-0.4.0+wasi-0.3.0-rc-2026-01-06
  (crate-source "wasip3" "0.4.0+wasi-0.3.0-rc-2026-01-06"
                "19dc8p0y2mfrvgk3qw3c3240nfbylv22mvyxz84dqpgai2zzha2l"))

;; Crate sources for clio dependencies

(define rust-ahash-0.8.12
  (crate-source "ahash" "0.8.12"
                "0xbsp9rlm5ki017c0w6ay8kjwinwm8knjncci95mii30rmwz25as"))

(define rust-android-system-properties-0.1.5
  (crate-source "android_system_properties" "0.1.5"
                "04b3wrz12837j7mdczqd95b732gw5q7q66cv4yn4646lvccp57l1"))

(define rust-arboard-3.6.1
  (crate-source "arboard" "3.6.1"
                "1byx6q5iipxkb0pyjp80k7c4akp4n5m7nsmqdbz4n7s9ak0a2j03"))

(define rust-arrayref-0.3.9
  (crate-source "arrayref" "0.3.9"
                "1jzyp0nvp10dmahaq9a2rnxqdd5wxgbvp8xaibps3zai8c9fi8kn"))

(define rust-arrayvec-0.7.6
  (crate-source "arrayvec" "0.7.6"
                "0l1fz4ccgv6pm609rif37sl5nv5k6lbzi7kkppgzqzh1vwix20kw"))

(define rust-assert-cmd-2.1.2
  (crate-source "assert_cmd" "2.1.2"
                "0505wrwzjfy2wdqhvmk0an4s69vbxfp5a45i5k8mvi4sfjlcynww"))

(define rust-blake3-1.8.3
  (crate-source "blake3" "1.8.3"
                "0b9ay320z90xs5hyk48l1v3208yyvdy3gs3nnlb7xyxkaxyyys14"))

(define rust-block2-0.6.2
  (crate-source "block2" "0.6.2"
                "1xcfllzx6c3jc554nmb5qy6xmlkl6l6j5ib4wd11800n0n3rvsyd"))

(define rust-bstr-1.12.1
  (crate-source "bstr" "1.12.1"
                "1arc1v7h5l86vd6z76z3xykjzldqd5icldn7j9d3p7z6x0d4w133"))

(define rust-bytemuck-1.25.0
  (crate-source "bytemuck" "1.25.0"
                "1v1z32igg9zq49phb3fra0ax5r2inf3aw473vldnm886sx5vdvy8"))

(define rust-byteorder-lite-0.1.0
  (crate-source "byteorder-lite" "0.1.0"
                "15alafmz4b9az56z6x7glcbcb6a8bfgyd109qc3bvx07zx4fj7wg"))

(define rust-chrono-0.4.43
  (crate-source "chrono" "0.4.43"
                "06312amlyys4kkjazl13mbxw0j2f7zxygzjkr1yk7s2sn57p9i7s"))

(define rust-clipboard-win-5.4.1
  (crate-source "clipboard-win" "5.4.1"
                "1m44gqy11rq1ww7jls86ppif98v6kv2wkwk8p17is86zsdq3gq5x"))

(define rust-constant-time-eq-0.4.2
  (crate-source "constant_time_eq" "0.4.2"
                "16zamq60dq80k3rqlzh9j9cpjhishmh924lnwbplgrnmkkvfylix"))

(define rust-cpufeatures-0.2.17
  (crate-source "cpufeatures" "0.2.17"
                "10023dnnaghhdl70xcds12fsx2b966sxbxjq5sxs49mvxqw5ivar"))

(define rust-crunchy-0.2.4
  (crate-source "crunchy" "0.2.4"
                "1mbp5navim2qr3x48lyvadqblcxc1dm0lqr0swrkkwy2qblvw3s6"))

(define rust-ctrlc-3.5.1
  (crate-source "ctrlc" "3.5.1"
                "146p40m5mj6w4nncj3wpsh0dlm0r0rjyblifp8sk1xxgqj4nlwvk"))

(define rust-difflib-0.4.0
  (crate-source "difflib" "0.4.0"
                "1s7byq4d7jgf2hcp2lcqxi2piqwl8xqlharfbi8kf90n8csy7131"))

(define rust-directories-5.0.1
  (crate-source "directories" "5.0.1"
                "0dba6xzk79s1clqzxh2qlgzk3lmvvks1lzzjhhi3hd70hhxifjcs"))

(define rust-dispatch2-0.3.0
  (crate-source "dispatch2" "0.3.0"
                "1v1ak9w0s8z1g13x4mj2y5im9wmck0i2vf8f8wc9l1n6lqi9z849"))

(define rust-downcast-rs-1.2.1
  (crate-source "downcast-rs" "1.2.1"
                "1lmrq383d1yszp7mg5i7i56b17x2lnn3kb91jwsq0zykvg2jbcvm"))

(define rust-error-code-3.3.2
  (crate-source "error-code" "3.3.2"
                "0nacxm9xr3s1rwd6fabk3qm89fyglahmbi4m512y0hr8ym6dz8ny"))

(define rust-env-filter-1.0.0
  (crate-source "env_filter" "1.0.0"
                "13rhwy5arjn626a0z3hvvkpf9w9pnll14c35vscyqx3jwp43q73s"))

(define rust-env-logger-0.11.9
  (crate-source "env_logger" "0.11.9"
                "13913sqpnhv741z5ixmcy5j3nnml53gmsllnhajjkx2ili7fxnmj"))

(define rust-fallible-iterator-0.3.0
  (crate-source "fallible-iterator" "0.3.0"
                "0ja6l56yka5vn4y4pk6hn88z0bpny7a8k1919aqjzp0j1yhy9k1a"))

(define rust-fallible-streaming-iterator-0.1.9
  (crate-source "fallible-streaming-iterator" "0.1.9"
                "0nj6j26p71bjy8h42x6jahx1hn0ng6mc2miwpgwnp8vnwqf4jq3k"))

(define rust-fax-0.2.6
  (crate-source "fax" "0.2.6"
                "1ax0jmvsszxd03hj6ga1kyl7gaqcfw0akg2wf0q6gk9pizaffpgh"))

(define rust-fax-derive-0.2.0
  (crate-source "fax_derive" "0.2.0"
                "0zap434zz4xvi5rnysmwzzivig593b4ng15vwzwl7js2nw7s3b50"))

(define rust-fdeflate-0.3.7
  (crate-source "fdeflate" "0.3.7"
                "130ga18vyxbb5idbgi07njymdaavvk6j08yh1dfarm294ssm6s0y"))

(define rust-fixedbitset-0.5.7
  (crate-source "fixedbitset" "0.5.7"
                "16fd3v9d2cms2vddf9xhlm56sz4j0zgrk3d2h6v1l7hx760lwrqx"))

(define rust-gethostname-1.1.0
  (crate-source "gethostname" "1.1.0"
                "1n6bj9gh503ggjblfjcai96gmxynxsrykaynljlrfdra34q95m0v"))

(define rust-half-2.7.1
  (crate-source "half" "2.7.1"
                "0jyq42xfa6sghc397mx84av7fayd4xfxr4jahsqv90lmjr5xi8kf"))

(define rust-hashbrown-0.14.5
  (crate-source "hashbrown" "0.14.5"
                "1wa1vy1xs3mp11bn3z9dv0jricgr6a2j0zkf1g19yz3vw4il89z5"))

(define rust-hashlink-0.9.1
  (crate-source "hashlink" "0.9.1"
                "1byq4nyrflm5s6wdx5qwp96l1qbp2d0nljvrr5yqrsfy51qzz93b"))

(define rust-iana-time-zone-0.1.65
  (crate-source "iana-time-zone" "0.1.65"
                "0w64khw5p8s4nzwcf36bwnsmqzf61vpwk9ca1920x82bk6nwj6z3"))

(define rust-iana-time-zone-haiku-0.1.2
  (crate-source "iana-time-zone-haiku" "0.1.2"
                "17r6jmj31chn7xs9698r122mapq85mfnv98bb4pg6spm0si2f67k"))

(define rust-image-0.25.9
  (crate-source "image" "0.25.9"
                "06lwa4ag3zcmjzivl356q0qhgxxqpkp7qwda7x0mjrkq21n6ql76"))

(define rust-js-sys-0.3.87
  (crate-source "js-sys" "0.3.87"
                "08gvld8jlhasnkrm719jczypwkdzggmj5k3daxccgbnsh4iqdw4k"))

(define rust-libredox-0.1.12
  (crate-source "libredox" "0.1.12"
                "05h6fb2y05h74zwaafmnf7gv3bxilzp7syqlfzw524w55kh9a2rx"))

(define rust-libsqlite3-sys-0.30.1
  (crate-source "libsqlite3-sys" "0.30.1"
                "0jcikvgbj84xc7ikdmpc8m4y5lyqgrb9aqblphwk67kv95xgp69f"))

(define rust-lock-api-0.4.14
  (crate-source "lock_api" "0.4.14"
                "0rg9mhx7vdpajfxvdjmgmlyrn20ligzqvn8ifmaz7dc79gkrjhr2"))

(define rust-moxcms-0.7.11
  (crate-source "moxcms" "0.7.11"
                "15qa5znj029i7677l0hdv0lwmjggrg920bhjgs3cjvydb72mg5dc"))

(define rust-nix-0.30.1
  (crate-source "nix" "0.30.1"
                "1dixahq9hk191g0c2ydc0h1ppxj0xw536y6rl63vlnp06lx3ylkl"))

(define rust-nom-8.0.0
  (crate-source "nom" "8.0.0"
                "01cl5xng9d0gxf26h39m0l8lprgpa00fcc75ps1yzgbib1vn35yz"))

(define rust-objc2-0.6.3
  (crate-source "objc2" "0.6.3"
                "01ccrb558qav2rqrmk0clzqzdd6r1rmicqnf55xqam7cw2f5khmp"))

(define rust-objc2-app-kit-0.3.2
  (crate-source "objc2-app-kit" "0.3.2"
                "132ijwni8lsi8phq7wnmialkxp46zx998fns3zq5np0ya1mr77nl"))

(define rust-objc2-core-foundation-0.3.2
  (crate-source "objc2-core-foundation" "0.3.2"
                "0dnmg7606n4zifyjw4ff554xvjmi256cs8fpgpdmr91gckc0s61a"))

(define rust-objc2-core-graphics-0.3.2
  (crate-source "objc2-core-graphics" "0.3.2"
                "01x8413pxq0m5rwidlaczni8v5cz9dc3xqzq8l9zlpl9cv8cj8p0"))

(define rust-objc2-encode-4.1.0
  (crate-source "objc2-encode" "4.1.0"
                "0cqckp4cpf68mxyc2zgnazj8klv0z395nsgbafa61cjgsyyan9gg"))

(define rust-objc2-foundation-0.3.2
  (crate-source "objc2-foundation" "0.3.2"
                "0wijkxzzvw2xkzssds3fj8279cbykz2rz9agxf6qh7y2agpsvq73"))

(define rust-objc2-io-surface-0.3.2
  (crate-source "objc2-io-surface" "0.3.2"
                "07fqx4fmwydf2arrc4xs4awv7zyzzxh60fyqdfmrpm9n148qh1qq"))

(define rust-os-pipe-1.2.3
  (crate-source "os_pipe" "1.2.3"
                "0rqrvm7fdp790b4ks3kcdzsgkz2528xrn3vxc9l4nf1inj2ax3vx"))

(define rust-parking-lot-0.12.5
  (crate-source "parking_lot" "0.12.5"
                "06jsqh9aqmc94j2rlm8gpccilqm6bskbd67zf6ypfc0f4m9p91ck"))

(define rust-parking-lot-core-0.9.12
  (crate-source "parking_lot_core" "0.9.12"
                "1hb4rggy70fwa1w9nb0svbyflzdc69h047482v2z3sx2hmcnh896"))

(define rust-petgraph-0.8.3
  (crate-source "petgraph" "0.8.3"
                "0mblnaqbx1y20h5y7pz6y11hk9jjk6k87lsmn7jxaq3hm67ba0c7"))

(define rust-png-0.18.1
  (crate-source "png" "0.18.1"
                "0qca282xp8a6d7mikxrwji3f52mjn4vnqxz2v9iz5adj665rnxk0"))

(define rust-predicates-3.1.4
  (crate-source "predicates" "3.1.4"
                "1ziwwshyl5d7yf9anyb8ldamqrx0kv1w3mhdnzkpx8i85y9z5a5d"))

(define rust-predicates-core-1.0.10
  (crate-source "predicates-core" "1.0.10"
                "0i6ia05imr1fsppc1z2lg0g2kpalz7crmlx0n4ql0sqnyd38glya"))

(define rust-predicates-tree-1.0.13
  (crate-source "predicates-tree" "1.0.13"
                "1wp2farzvl4aarpa3sdq59bd1rk0zzqrszj6n0fi7j1rgf21ppnh"))

(define rust-pxfm-0.1.27
  (crate-source "pxfm" "0.1.27"
                "1a76ydn3wpl2dvyzplv3c6fkx4mkjc9ns60xas9l7alk4n1d71ki"))

(define rust-quick-error-2.0.1
  (crate-source "quick-error" "2.0.1"
                "18z6r2rcjvvf8cn92xjhm2qc3jpd1ljvcbf12zv0k9p565gmb4x9"))

(define rust-quick-xml-0.38.4
  (crate-source "quick-xml" "0.38.4"
                "0772siy4d9vlq77842012c8cycs3y0szxkv62rh9sh2sqmc20v5n"))

(define rust-redox-syscall-0.5.18
  (crate-source "redox_syscall" "0.5.18"
                "0b9n38zsxylql36vybw18if68yc9jczxmbyzdwyhb9sifmag4azd"))

(define rust-rusqlite-0.32.1
  (crate-source "rusqlite" "0.32.1"
                "0vlx040bppl414pbjgbp7qr4jdxwszi9krx0m63zzf2f2whvflvp"))

(define rust-rusqlite-migration-1.3.1
  (crate-source "rusqlite_migration" "1.3.1"
                "076dm65g0sngzrb93r07va4l5zl3gjx9gq5mlsh21p7p0bl44fwj"))

(define rust-scopeguard-1.2.0
  (crate-source "scopeguard" "1.2.0"
                "0jcz9sd47zlsgcnm1hdw0664krxwb5gczlif4qngj2aif8vky54l"))

(define rust-termtree-0.5.1
  (crate-source "termtree" "0.5.1"
                "10s610ax6nb70yi7xfmwcb6d3wi9sj5isd0m63gy2pizr2zgwl4g"))

(define rust-tiff-0.10.3
  (crate-source "tiff" "0.10.3"
                "0vrkdk9cdk07rh7iifcxpn6m8zv3wz695mizhr8rb3gfgzg0b5mg"))

(define rust-tree-magic-mini-3.2.2
  (crate-source "tree_magic_mini" "3.2.2"
                "19nm2hkspb8p4gxgk442b1hmbbh9l5fnf7w3nli6rfhw0s85nxmq"))

(define rust-wasm-bindgen-0.2.110
  (crate-source "wasm-bindgen" "0.2.110"
                "0rjqqgb1salkknpxn33yqkpcdka0n6g075zhhjyr37baqv6l3qhx"))

(define rust-wasm-bindgen-macro-0.2.110
  (crate-source "wasm-bindgen-macro" "0.2.110"
                "0llaphxgmkgmvzbzzfp0w3gdx73pxlygzmfpwqyjx3q495kdybz1"))

(define rust-wasm-bindgen-macro-support-0.2.110
  (crate-source "wasm-bindgen-macro-support" "0.2.110"
                "02dk9yy1f1ri5h3zal8gqj0z9ns6znbf7h9l4d3c6njnbiwx2qqf"))

(define rust-wasm-bindgen-shared-0.2.110
  (crate-source "wasm-bindgen-shared" "0.2.110"
                "0lybp5l8qmmqqsha47krj8c527jhia4j924zpzv73km54fncvyg9"))

(define rust-wayland-backend-0.3.12
  (crate-source "wayland-backend" "0.3.12"
                "1yb4s5mbcis3z3gcmxq2lzgrcw2li7jsfr9ayi4gcsyrrja43rpy"))

(define rust-wayland-client-0.31.12
  (crate-source "wayland-client" "0.31.12"
                "1v1b2b2s0ld41psn3v2p3c6i590iz3r427czrf3c3dpv6yjzmrmq"))

(define rust-wayland-protocols-0.32.10
  (crate-source "wayland-protocols" "0.32.10"
                "1wzl7ly3ahi2y4swf8wmlqaj3gck4fpmwf6ymbfxd37wpkzskvds"))

(define rust-wayland-protocols-wlr-0.3.10
  (crate-source "wayland-protocols-wlr" "0.3.10"
                "1ws5fd7qs5vf3digbnn20n7mks2sdg76sy13b36k836g0bgpqng9"))

(define rust-wayland-scanner-0.31.8
  (crate-source "wayland-scanner" "0.31.8"
                "1qw971z9jcxdw8s371gx2anmwb95m59y38q3k11qxrk3d95yj8sl"))

(define rust-wayland-sys-0.31.8
  (crate-source "wayland-sys" "0.31.8"
                "1zdxrcl8paklwir0lag1i80k6h0iq1f80d925b4p9yaymk1vyv8y"))

(define rust-weezl-0.1.12
  (crate-source "weezl" "0.1.12"
                "122a1dhha6cib5az4ihcqlh60ns2bi6rskdv875p94lbvj6wk2m2"))

(define rust-windows-aarch64-gnullvm-0.53.1
  (crate-source "windows_aarch64_gnullvm" "0.53.1"
                "0lqvdm510mka9w26vmga7hbkmrw9glzc90l4gya5qbxlm1pl3n59"))

(define rust-windows-aarch64-msvc-0.53.1
  (crate-source "windows_aarch64_msvc" "0.53.1"
                "01jh2adlwx043rji888b22whx4bm8alrk3khjpik5xn20kl85mxr"))

(define rust-windows-core-0.62.2
  (crate-source "windows-core" "0.62.2"
                "1swxpv1a8qvn3bkxv8cn663238h2jccq35ff3nsj61jdsca3ms5q"))

(define rust-windows-i686-gnu-0.53.1
  (crate-source "windows_i686_gnu" "0.53.1"
                "18wkcm82ldyg4figcsidzwbg1pqd49jpm98crfz0j7nqd6h6s3ln"))

(define rust-windows-i686-gnullvm-0.53.1
  (crate-source "windows_i686_gnullvm" "0.53.1"
                "030qaxqc4salz6l4immfb6sykc6gmhyir9wzn2w8mxj8038mjwzs"))

(define rust-windows-i686-msvc-0.53.1
  (crate-source "windows_i686_msvc" "0.53.1"
                "1hi6scw3mn2pbdl30ji5i4y8vvspb9b66l98kkz350pig58wfyhy"))

(define rust-windows-implement-0.60.2
  (crate-source "windows-implement" "0.60.2"
                "1psxhmklzcf3wjs4b8qb42qb6znvc142cb5pa74rsyxm1822wgh5"))

(define rust-windows-interface-0.59.3
  (crate-source "windows-interface" "0.59.3"
                "0n73cwrn4247d0axrk7gjp08p34x1723483jxjxjdfkh4m56qc9z"))

(define rust-windows-result-0.4.1
  (crate-source "windows-result" "0.4.1"
                "1d9yhmrmmfqh56zlj751s5wfm9a2aa7az9rd7nn5027nxa4zm0bp"))

(define rust-windows-strings-0.5.1
  (crate-source "windows-strings" "0.5.1"
                "14bhng9jqv4fyl7lqjz3az7vzh8pw0w4am49fsqgcz67d67x0dvq"))

(define rust-windows-targets-0.53.5
  (crate-source "windows-targets" "0.53.5"
                "1wv9j2gv3l6wj3gkw5j1kr6ymb5q6dfc42yvydjhv3mqa7szjia9"))

(define rust-windows-x86-64-gnu-0.53.1
  (crate-source "windows_x86_64_gnu" "0.53.1"
                "16d4yiysmfdlsrghndr97y57gh3kljkwhfdbcs05m1jasz6l4f4w"))

(define rust-windows-x86-64-gnullvm-0.53.1
  (crate-source "windows_x86_64_gnullvm" "0.53.1"
                "1qbspgv4g3q0vygkg8rnql5c6z3caqv38japiynyivh75ng1gyhg"))

(define rust-windows-x86-64-msvc-0.53.1
  (crate-source "windows_x86_64_msvc" "0.53.1"
                "0l6npq76vlq4ksn4bwsncpr8508mk0gmznm6wnhjg95d19gzzfyn"))

(define rust-wl-clipboard-rs-0.9.3
  (crate-source "wl-clipboard-rs" "0.9.3"
                "18xh5q3r9k57v3g2565vr33irldjh99p29x1ydpdk1rfldqi8rg9"))

(define rust-x11rb-0.13.2
  (crate-source "x11rb" "0.13.2"
                "053lvnaw9ycbl791mgwly2hw27q6vqgzrb1y5kz1as52wmdsm4wr"))

(define rust-x11rb-protocol-0.13.2
  (crate-source "x11rb-protocol" "0.13.2"
                "1g81cznbyn522b0fbis0i44wh3adad2vhsz5pzf99waf3sbc4vza"))

(define rust-zune-core-0.4.12
  (crate-source "zune-core" "0.4.12"
                "0jj1ra86klzlcj9aha9als9d1dzs7pqv3azs1j3n96822wn3lhiz"))

(define rust-zune-jpeg-0.4.21
  (crate-source "zune-jpeg" "0.4.21"
                "04r7g6y9jp7d4c9bq23rz3gwzlr1dsl7vdk4yly35bc4jf52rki9"))

;; ast-index dependencies
(define rust-anes-0.1.6
  (crate-source "anes" "0.1.6"
                "16bj1ww1xkwzbckk32j2pnbn5vk6wgsl3q4p3j9551xbcarwnijb"))

(define rust-anyhow-1.0.100
  (crate-source "anyhow" "1.0.100"
                "0qbfmw4hhv2ampza1csyvf1jqjs2dgrj29cv3h3sh623c6qvcgm2"))

(define rust-bitflags-2.10.0
  (crate-source "bitflags" "2.10.0"
                "1lqxwc3625lcjrjm5vygban9v8a6dlxisp1aqylibiaw52si4bl1"))

(define rust-bumpalo-3.19.1
  (crate-source "bumpalo" "3.19.1"
                "044555i277xcinmqs7nnv8n5y4fqfi4l4lp1mp3i30vsidrxrnax"))

(define rust-cc-1.2.53
  (crate-source "cc" "1.2.53"
                "0cjrx2nzlz8l93p4sfymjgsv0jicvfphd6hyhk5gyxbi2z72ypbm"))

(define rust-ciborium-0.2.2
  (crate-source "ciborium" "0.2.2"
                "03hgfw4674im1pdqblcp77m7rc8x2v828si5570ga5q9dzyrzrj2"))

(define rust-ciborium-io-0.2.2
  (crate-source "ciborium-io" "0.2.2"
                "0my7s5g24hvp1rs1zd1cxapz94inrvqpdf1rslrvxj8618gfmbq5"))

(define rust-ciborium-ll-0.2.2
  (crate-source "ciborium-ll" "0.2.2"
                "1n8g4j5rwkfs3rzfi6g1p7ngmz6m5yxsksryzf5k72ll7mjknrjp"))

(define rust-clap-4.5.54
  (crate-source "clap" "4.5.54"
                "15737jmai272j6jh4ha4dq4ap14ysx2sa5wsjv6zbkvrrnfzzrn6"))

(define rust-clap-builder-4.5.54
  (crate-source "clap_builder" "4.5.54"
                "001cnl5ccva6z3x5nw3m72zs3bzb650anz1scs7vqhbs5d6wyhps"))

(define rust-clap-derive-4.5.49
  (crate-source "clap_derive" "4.5.49"
                "0wbngw649138v3jwx8pm5x9sq0qsml3sh0sfzyrdxcpamy3m82ra"))

(define rust-clap-lex-0.7.7
  (crate-source "clap_lex" "0.7.7"
                "0cibsbziyzw2ywar2yh6zllsamhwkblfly565zgi56s3q064prn3"))

(define rust-colored-2.2.0
  (crate-source "colored" "2.2.0"
                "0g6s7j2qayjd7i3sivmwiawfdg8c8ldy0g2kl4vwk1yk16hjaxqi"))

(define rust-criterion-0.5.1
  (crate-source "criterion" "0.5.1"
                "0bv9ipygam3z8kk6k771gh9zi0j0lb9ir0xi1pc075ljg80jvcgj"))

(define rust-criterion-plot-0.5.0
  (crate-source "criterion-plot" "0.5.0"
                "1c866xkjqqhzg4cjvg01f8w6xc1j3j7s58rdksl52skq89iq4l3b"))

(define rust-crossbeam-channel-0.5.15
  (crate-source "crossbeam-channel" "0.5.15"
                "1cicd9ins0fkpfgvz9vhz3m9rpkh6n8d3437c3wnfsdkd3wgif42"))

(define rust-crossbeam-deque-0.8.6
  (crate-source "crossbeam-deque" "0.8.6"
                "0l9f1saqp1gn5qy0rxvkmz4m6n7fc0b3dbm6q1r5pmgpnyvi3lcx"))

(define rust-csv-1.4.0
  (crate-source "csv" "1.4.0"
                "0f7r2ip0rbi7k377c3xmsh9xd69sillffhpfmbgnvz3yrxl9vkaj"))

(define rust-csv-core-0.1.13
  (crate-source "csv-core" "0.1.13"
                "10lppd3fdb1i5npgx9xqjs5mjmy2qbdi8n16i48lg03ak4k3qjkh"))

(define rust-deranged-0.5.5
  (crate-source "deranged" "0.5.5"
                "11z5939gv2klp1r1lgrp4w5fnlkj18jqqf0h9zxmia3vkrjwpv7c"))

(define rust-filetime-0.2.27
  (crate-source "filetime" "0.2.27"
                "1nspbkm1d1km7xfljcbl565swqxrihqyin8bqppig2gf3qal927r"))

(define rust-find-msvc-tools-0.1.8
  (crate-source "find-msvc-tools" "0.1.8"
                "1nv8hn78xphg04l6w7iq1v8lsmmqx6ripbig18qn92m9r2yb14c5"))

(define rust-fs2-0.4.3
  (crate-source "fs2" "0.4.3"
                "04v2hwk7035c088f19mfl5b1lz84gnvv2hv6m935n0hmirszqr4m"))

(define rust-fsevent-sys-4.1.0
  (crate-source "fsevent-sys" "4.1.0"
                "1liz67v8b0gcs8r31vxkvm2jzgl9p14i78yfqx81c8sdv817mvkn"))

(define rust-globset-0.4.18
  (crate-source "globset" "0.4.18"
                "1qsp3wg0mgxzmshcgymdlpivqlc1bihm6133pl6dx2x4af8w3psj"))

(define rust-grep-matcher-0.1.8
  (crate-source "grep-matcher" "0.1.8"
                "08w0i8iai5y672fp3fhqnpmlmbk663gxfh0bg0nv4nijjc8bgmrn"))

(define rust-grep-regex-0.1.14
  (crate-source "grep-regex" "0.1.14"
                "1vqjf7dk8lk9jr7i45cf5q99ily1bsj1ab41gg0br0mdqdbc5q0c"))

(define rust-grep-searcher-0.1.16
  (crate-source "grep-searcher" "0.1.16"
                "0d6wfw2vr8n2pwqzar4fi0c670axj13q2d151arfnj6w499jjqxc"))

(define rust-hermit-abi-0.5.2
  (crate-source "hermit-abi" "0.5.2"
                "1744vaqkczpwncfy960j2hxrbjl1q01csm84jpd9dajbdr2yy3zw"))

(define rust-iana-time-zone-0.1.64
  (crate-source "iana-time-zone" "0.1.64"
                "1yz980fmhaq9bdkasz35z63az37ci6kzzfhya83kgdqba61pzr9k"))

(define rust-ignore-0.4.25
  (crate-source "ignore" "0.4.25"
                "0jlv2s4fxqj9fsz6y015j5vbz6i475hj80j9q3sy05d0cniq5myk"))

(define rust-inotify-0.10.2
  (crate-source "inotify" "0.10.2"
                "1k2m6a95827yspax1icmwiz4szr7c01w3dnn2b2bil4hfvcnilgx"))

(define rust-inotify-sys-0.1.5
  (crate-source "inotify-sys" "0.1.5"
                "1syhjgvkram88my04kv03s0zwa66mdwa5v7ddja3pzwvx2sh4p70"))

(define rust-instant-0.1.13
  (crate-source "instant" "0.1.13"
                "08h27kzvb5jw74mh0ajv0nv9ggwvgqm8ynjsn2sa9jsks4cjh970"))

(define rust-is-terminal-0.4.17
  (crate-source "is-terminal" "0.4.17"
                "0ilfr9n31m0k6fsm3gvfrqaa62kbzkjqpwcd9mc46klfig1w2h1n"))

(define rust-kqueue-1.1.1
  (crate-source "kqueue" "1.1.1"
                "0sjrsnza8zxr1zfpv6sa0zapd54kx9wlijrz9apqvs6wsw303hza"))

(define rust-kqueue-sys-1.0.4
  (crate-source "kqueue-sys" "1.0.4"
                "12w3wi90y4kwis4k9g6fp0kqjdmc6l00j16g8mgbhac7vbzjb5pd"))

(define rust-libc-0.2.180
  (crate-source "libc" "0.2.180"
                "1z2n7hl10fnk1xnv19ahhqxwnb4qi9aclnl6gigim2aaahw5mhxw"))

(define rust-libsqlite3-sys-0.28.0
  (crate-source "libsqlite3-sys" "0.28.0"
                "0gzwfw0n2wqgaihcgj65wzd3lclfxyy62gixq8sv6z04fi15h40c"))

(define rust-memchr-2.7.6
  (crate-source "memchr" "2.7.6"
                "0wy29kf6pb4fbhfksjbs05jy2f32r2f3r1ga6qkmpz31k79h0azm"))

(define rust-memmap2-0.9.9
  (crate-source "memmap2" "0.9.9"
                "146lfx0mpib44wvws6hibahm4h2w867bzwsc6zhmi9p0l3j36hbl"))

(define rust-mio-1.1.1
  (crate-source "mio" "1.1.1"
                "1z2phpalqbdgihrcjp8y09l3kgq6309jnhnr6h11l9s7mnqcm6x6"))

(define rust-notify-7.0.0
  (crate-source "notify" "7.0.0"
                "02a0a1n0raxqslwhfprwmz7w34v54r42006q0m8bmy89jz1v8cy5"))

(define rust-notify-debouncer-mini-0.5.0
  (crate-source "notify-debouncer-mini" "0.5.0"
                "16f7r2mmrwapfmn5dxs389zla6sdmgsxr55yhbkxr5zd0xnsd9da"))

(define rust-notify-types-1.0.1
  (crate-source "notify-types" "1.0.1"
                "0x5idrpxzf70ng88rz28dqmgx1jyddf0vxx1x3csw09fw6skqpaq"))

(define rust-proc-macro2-1.0.105
  (crate-source "proc-macro2" "1.0.105"
                "1rvgs5qdznlrqrgicmv24nybnrnv8kyvk2vi7s52ddna1q71hpak"))

(define rust-quote-1.0.43
  (crate-source "quote" "1.0.43"
                "02n41mlr81qmczac7m5kjy51y8b7yrb8ym4ncmjycampjjjxjx6w"))

(define rust-rayon-1.11.0
  (crate-source "rayon" "1.11.0"
                "13x5fxb7rn4j2yw0cr26n7782jkc7rjzmdkg42qxk3xz0p8033rn"))

(define rust-rayon-core-1.13.0
  (crate-source "rayon-core" "1.13.0"
                "14dbr0sq83a6lf1rfjq5xdpk5r6zgzvmzs5j6110vlv2007qpq92"))

(define rust-redox-syscall-0.7.0
  (crate-source "redox_syscall" "0.7.0"
                "09zfw2jp6hgpn5pkayv9wh01sw410566qk8zwkljm7p6i44gxws9"))

(define rust-regex-1.12.2
  (crate-source "regex" "1.12.2"
                "1m14zkg6xmkb0q5ah3y39cmggclsjdr1wpxfa4kf5wvm3wcw0fw4"))

(define rust-regex-automata-0.4.13
  (crate-source "regex-automata" "0.4.13"
                "070z0j23pjfidqz0z89id1fca4p572wxpcr20a0qsv68bbrclxjj"))

(define rust-regex-syntax-0.8.8
  (crate-source "regex-syntax" "0.8.8"
                "0n7ggnpk0r32rzgnycy5xrc1yp2kq19m6pz98ch3c6dkaxw9hbbs"))

(define rust-rusqlite-0.31.0
  (crate-source "rusqlite" "0.31.0"
                "1bic69apqidimqf8gm80b98a832qzl9x6ns8myzah4yjg2ifnf5q"))

(define rust-ryu-1.0.22
  (crate-source "ryu" "1.0.22"
                "1139acr2kd4n8p36bp1n42xrpaphn6dhwklnazh8hpdnfps4q3x5"))

(define rust-streaming-iterator-0.1.9
  (crate-source "streaming-iterator" "0.1.9"
                "0845zdv8qb7zwqzglpqc0830i43xh3fb6vqms155wz85qfvk28ib"))

(define rust-syn-2.0.114
  (crate-source "syn" "2.0.114"
                "0akw62dizhyrkf3ym1jsys0gy1nphzgv0y8qkgpi6c1s4vghglfl"))

(define rust-tempfile-3.24.0
  (crate-source "tempfile" "3.24.0"
                "171fz3h6rj676miq15fyv1hnv69p426mlp8489bwa1b3xg3sjpb5"))

(define rust-time-0.3.45
  (crate-source "time" "0.3.45"
                "1gdag88agck220k6fxbgb7gsnr2r14n33sxzm5db9zfp6gy45r7r"))

(define rust-time-core-0.1.7
  (crate-source "time-core" "0.1.7"
                "1jilglvr6m6h2iidnvdp3zfahck9wa7kw64rslk79v1izncfwdlb"))

(define rust-time-macros-0.2.25
  (crate-source "time-macros" "0.2.25"
                "1pg3zrqyvjcy1nh470mdw7qxwwq6zmwq3f1dlp11mxlv4k8m5rbi"))

(define rust-tree-sitter-0.19.5
  (crate-source "tree-sitter" "0.19.5"
                "1h6adq5kqf4izzsklch5lfxx2aisxga463zz7w44rgwnck16wwmd"))

(define rust-tree-sitter-0.26.5
  (crate-source "tree-sitter" "0.26.5"
                "0fgy5qml2p2i51g132wa1hr9zvmasmzch3d20s9rpz2fymqp760j"))

(define rust-tree-sitter-bash-0.25.1
  (crate-source "tree-sitter-bash" "0.25.1"
                "0qihqn7nska917s2fc8q1pa0lsxjvsjxiw1x3mb1pjcw4xlwfply"))

(define rust-tree-sitter-c-sharp-0.23.1
  (crate-source "tree-sitter-c-sharp" "0.23.1"
                "1c7w6wvjc54k6kh0qrlspm9ksr4y10aq4fv6b0bkaibvrb66mw37"))

(define rust-tree-sitter-cpp-0.23.4
  (crate-source "tree-sitter-cpp" "0.23.4"
                "0hs7p45av437iw8rzsyw46qs06axbam7wadr655apd27kpm9c8fz"))

(define rust-tree-sitter-dart-0.0.4
  (crate-source "tree-sitter-dart" "0.0.4"
                "12p3yhv81fxsmd55nlw7jpl9sbmsnmkyzz5a2hz38hffh05zgw8r"))

(define rust-tree-sitter-elixir-0.3.4
  (crate-source "tree-sitter-elixir" "0.3.4"
                "0grdkbx6bqw3s1w3mkk94sibmhgdicdlqirjzpc57zdl8x348pg4"))

(define rust-tree-sitter-go-0.25.0
  (crate-source "tree-sitter-go" "0.25.0"
                "1shnigi37lrq88b93i1vnha62byy1nykrq62sbac0p435x6hlmn8"))

(define rust-tree-sitter-groovy-0.1.2
  (crate-source "tree-sitter-groovy" "0.1.2"
                "1va07jhqgq7snx5fd57ripgfb7a2j97kal6a0almp1ph2xh0282s"))

(define rust-tree-sitter-java-0.23.5
  (crate-source "tree-sitter-java" "0.23.5"
                "1mlh3skj2nasrwdz0v865r4hxnk7v8037z8nwqab4yf6r36wp9ha"))

(define rust-tree-sitter-kotlin-ng-1.1.0
  (crate-source "tree-sitter-kotlin-ng" "1.1.0"
                "0l243fswf9jjnqcyirjy56qr86d38x4w7lpl4krgpb1qm6yyn078"))

(define rust-tree-sitter-language-0.1.7
  (crate-source "tree-sitter-language" "0.1.7"
                "10hpwqd45v529p1q23d11k8wms7zifyda5s9yl7xa36ca3qr9680"))

(define rust-tree-sitter-lua-0.5.0
  (crate-source "tree-sitter-lua" "0.5.0"
                "035169zpbbnxmlsck6d26q6r4jsxzban15y30f3ab22i4gsgbald"))

(define rust-tree-sitter-objc-3.0.2
  (crate-source "tree-sitter-objc" "3.0.2"
                "1lp1570h6lwhknzq3nn9sf26cfkqbx99vrrm0mpigz13ciavpa4w"))

(define rust-tree-sitter-php-0.24.2
  (crate-source "tree-sitter-php" "0.24.2"
                "14jmvysx66irxjgpgvlp3dfw46yxfbcmrzx7x9g2q1b9mg1ig30d"))

(define rust-tree-sitter-proto-0.4.0
  (crate-source "tree-sitter-proto" "0.4.0"
                "077kg2lhzdaiyzlvy8mqjm1jbmfrgb7jwlcfgfzxdjx3bz5hqh9f"))

(define rust-tree-sitter-python-0.25.0
  (crate-source "tree-sitter-python" "0.25.0"
                "072anxf7f3wn2jzpa1c8fnnskhwjjkd4qvzlc2zl1rsjjv9mzy3b"))

(define rust-tree-sitter-r-1.2.0
  (crate-source "tree-sitter-r" "1.2.0"
                "0mbja7yin41q453ssp2qz1sjiln3nsmydapk7vh4d2lzvb5k74a2"))

(define rust-tree-sitter-ruby-0.23.1
  (crate-source "tree-sitter-ruby" "0.23.1"
                "15cz4h1sfgf838r2pmf7vg9ahh0kwgkvvnjgbdbrrfzn9vm8815y"))

(define rust-tree-sitter-rust-0.24.0
  (crate-source "tree-sitter-rust" "0.24.0"
                "1q8vqslcnp2pvyg06733ddag4i4w3jlv5s1bf8h28jk89h1ii6sb"))

(define rust-tree-sitter-scala-0.24.0
  (crate-source "tree-sitter-scala" "0.24.0"
                "0ldjl3cq5rvm1d6c61nx2i2l7cwkd7l67ca5627dw3pls6rsw5km"))

(define rust-tree-sitter-sql-0.0.2
  (crate-source "tree-sitter-sql" "0.0.2"
                "0bn8djk16myx5p29067casvkbdfjl8ygyisgl19q2lkn4lvzpr04"))

(define rust-tree-sitter-swift-0.7.1
  (crate-source "tree-sitter-swift" "0.7.1"
                "0n9fhg4imn0zzd4qa36gcq89nl4drd3z6dj7hvxg8g9y3h0idwjf"))

(define rust-tree-sitter-typescript-0.23.2
  (crate-source "tree-sitter-typescript" "0.23.2"
                "1zsyaxx3v1sd8gx2zkscwv6z1sq2nvccqpvd8k67ayllipnpcpvc"))

(define rust-unicode-ident-1.0.22
  (crate-source "unicode-ident" "1.0.22"
                "1x8xrz17vqi6qmkkcqr8cyf0an76ig7390j9cnqnk47zyv2gf4lk"))

(define rust-uuid-1.19.0
  (crate-source "uuid" "1.19.0"
                "0jjbclx3f36fjl6jjh8f022q0m76v3cfh61y6z6jgl2b3f359q72"))

(define rust-wasi-0.11.1+wasi-snapshot-preview1
  (crate-source "wasi" "0.11.1+wasi-snapshot-preview1"
                "0jx49r7nbkbhyfrfyhz0bm4817yrnxgd3jiwwwfv0zl439jyrwyc"))

(define rust-wasip2-1.0.2+wasi-0.2.9
  (crate-source "wasip2" "1.0.2+wasi-0.2.9"
                "1xdw7v08jpfjdg94sp4lbdgzwa587m5ifpz6fpdnkh02kwizj5wm"))

(define rust-zerocopy-0.8.33
  (crate-source "zerocopy" "0.8.33"
                "1z9d6z8p1ndf0yrvw99jr5zcjnd4270kv4rivqqyi7hbs5l533v6"))

(define rust-zerocopy-derive-0.8.33
  (crate-source "zerocopy-derive" "0.8.33"
                "1wbh4bil3kqfmiwxlpzhxba6fyh09nsy87k7idk8b1hadfr64y9c"))

(define rust-zmij-1.0.16
  (crate-source "zmij" "1.0.16"
                "0r8swld9cwnyvdfamq4063ngwxdzckg4922ayk7likma4mc19kfz"))


;; Define cargo inputs mapping
(define-cargo-inputs lookup-cargo-inputs
  (convco =>
          (list rust-addr2line-0.24.2
                rust-adler2-2.0.0
                rust-aho-corasick-1.1.3
                rust-anstream-0.6.18
                rust-anstyle-1.0.10
                rust-anstyle-parse-0.2.6
                rust-anstyle-query-1.1.2
                rust-anstyle-wincon-3.0.6
                rust-anyhow-1.0.95
                rust-backtrace-0.3.74
                rust-bitflags-2.6.0
                rust-block-buffer-0.10.4
                rust-cc-1.2.6
                rust-cfg-if-1.0.0
                rust-cfg-aliases-0.2.1
                rust-clap-4.5.27
                rust-clap-builder-4.5.27
                rust-clap-complete-4.5.43
                rust-clap-derive-4.5.24
                rust-clap-lex-0.7.4
                rust-cmake-0.1.52
                rust-colorchoice-1.0.3
                rust-console-0.15.10
                rust-cpufeatures-0.2.16
                rust-crypto-common-0.1.6
                rust-ctrlc-3.4.5
                rust-darling-0.20.10
                rust-darling-core-0.20.10
                rust-darling-macro-0.20.10
                rust-derive-builder-0.20.2
                rust-derive-builder-core-0.20.2
                rust-derive-builder-macro-0.20.2
                rust-dialoguer-0.11.0
                rust-digest-0.10.7
                rust-displaydoc-0.2.5
                rust-encode-unicode-1.0.0
                rust-equivalent-1.0.1
                rust-errno-0.3.10
                rust-fastrand-2.3.0
                rust-fnv-1.0.7
                rust-form-urlencoded-1.2.1
                rust-fuzzy-matcher-0.3.7
                rust-generic-array-0.14.7
                rust-gimli-0.31.1
                rust-git2-0.20.0
                rust-handlebars-6.3.0
                rust-hashbrown-0.15.2
                rust-heck-0.5.0
                rust-icu-collections-1.5.0
                rust-icu-locid-1.5.0
                rust-icu-locid-transform-1.5.0
                rust-icu-locid-transform-data-1.5.0
                rust-icu-normalizer-1.5.0
                rust-icu-normalizer-data-1.5.0
                rust-icu-properties-1.5.1
                rust-icu-properties-data-1.5.0
                rust-icu-provider-1.5.0
                rust-icu-provider-macros-1.5.0
                rust-ident-case-1.0.1
                rust-idna-1.0.3
                rust-idna-adapter-1.2.0
                rust-indexmap-2.7.0
                rust-is-terminal-polyfill-1.70.1
                rust-itoa-1.0.14
                rust-jiff-0.1.28
                rust-jiff-tzdb-0.1.2
                rust-jiff-tzdb-platform-0.1.2
                rust-jobserver-0.1.32
                rust-aho-corasick-1.1.3
                rust-atty-0.2.14
                rust-cast-0.3.0
                rust-clap-2.34.0
                rust-criterion-plot-0.4.5
                rust-csv-1.1.6
                rust-itertools-0.10.5
                rust-num-traits-0.2.19
                rust-cfg-if-1.0.0
                rust-fastrand-2.2.0
                rust-hermit-abi-0.3.0
                rust-once-cell-1.20.2
                rust-rustix-0.38.42
                rust-windows-sys-0.59.0
                rust-libc-0.2.169
                rust-libgit2-sys-0.18.0+1.9.0
                rust-regex-syntax-0.8.5
                rust-libssh2-sys-0.3.0
                rust-libz-sys-1.1.20
                rust-linux-raw-sys-0.4.14
                rust-litemap-0.7.4
                rust-log-0.4.22
                rust-memchr-2.7.4
                rust-miniz-oxide-0.8.2
                rust-nix-0.29.0
                rust-num-modular-0.6.1
                rust-num-order-1.2.0
                rust-object-0.36.7
                rust-once-cell-1.20.2
                rust-openssl-sys-0.9.104
                rust-percent-encoding-2.3.1
                rust-pest-2.7.15
                rust-pest-derive-2.7.15
                rust-pest-generator-2.7.15
                rust-pest-meta-2.7.15
                rust-pkg-config-0.3.31
                rust-portable-atomic-1.10.0
                rust-portable-atomic-util-0.2.4
                rust-proc-macro2-1.0.92
                rust-quote-1.0.38
                rust-regex-1.11.1
                rust-regex-automata-0.4.9
                rust-regex-syntax-0.8.5
                rust-rustc-demangle-0.1.24
                rust-rustix-0.38.42
                rust-ryu-1.0.18
                rust-same-file-1.0.6
                rust-semver-1.0.25
                rust-serde-1.0.217
                rust-serde-derive-1.0.217
                rust-serde-json-1.0.134
                rust-serde-norway-0.9.42
                rust-sha2-0.10.8
                rust-shell-words-1.1.0
                rust-shlex-1.3.0
                rust-smallvec-1.13.2
                rust-stable-deref-trait-1.2.0
                rust-strsim-0.11.1
                rust-syn-2.0.93
                rust-synstructure-0.13.1
                rust-tempfile-3.14.0
                rust-thiserror-1.0.69
                rust-thiserror-2.0.11
                rust-thiserror-impl-1.0.69
                rust-thiserror-impl-2.0.11
                rust-thread-local-1.1.8
                rust-tinystr-0.7.6
                rust-typenum-1.17.0
                rust-ucd-trie-0.1.7
                rust-unicode-ident-1.0.14
                rust-unicode-width-0.2.0
                rust-unsafe-libyaml-norway-0.2.15
                rust-url-2.5.4
                rust-utf16-iter-1.0.5
                rust-utf8-iter-1.0.4
                rust-utf8parse-0.2.2
                rust-vcpkg-0.2.15
                rust-version-check-0.9.5
                rust-walkdir-2.5.0
                rust-winapi-util-0.1.9
                rust-windows-sys-0.59.0
                rust-windows-targets-0.52.6
                rust-windows-aarch64-gnullvm-0.52.6
                rust-windows-aarch64-msvc-0.52.6
                rust-windows-i686-gnu-0.52.6
                rust-windows-i686-gnullvm-0.52.6
                rust-windows-i686-msvc-0.52.6
                rust-windows-x86-64-gnu-0.52.6
                rust-windows-x86-64-gnullvm-0.52.6
                rust-windows-x86-64-msvc-0.52.6
                rust-write16-1.0.0
                rust-writeable-0.5.5
                rust-yoke-0.7.5
                rust-yoke-derive-0.7.5
                rust-zerofrom-0.1.5
                rust-zerofrom-derive-0.1.5
                rust-zeroize-1.8.1
                rust-zerovec-0.10.4
                rust-zerovec-derive-0.10.3))
  (i3im =>
        (list rust-anyhow-1.0.95
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
  (shadowplay =>
              (list rust-ansi-term-0.12.1
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
                    rust-windows-x86-64-msvc-0.53.0))
  (xidlehook =>
             (list rust-clap-2.33.3
                   rust-env-logger-0.7.1
                   rust-libpulse-binding-2.23.0
                   rust-nix-0.15.0
                   rust-x11-2.18.2
                   rust-xcb-0.9.0
                   rust-xidlehook-core-0.3.0
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
  (ripsecrets =>
              (list rust-clap-4.5.27
                    rust-clap-builder-4.5.27
                    rust-clap-derive-4.5.24
                    rust-crossbeam-deque-0.8
                    rust-crossbeam-epoch-0.9.18
                    rust-globset-0.4
                    rust-grep-0.2
                    rust-grep-cli-0.1
                    rust-grep-matcher-0.1
                    rust-grep-printer-0.1
                    rust-grep-regex-0.1
                    rust-grep-searcher-0.1
                    rust-ignore-0.4
                    rust-aho-corasick-0.7.20
                    rust-aho-corasick-1.1.3
                    rust-atty-0.2.14
                    rust-num-cpus-1
                    rust-memoize-0.3
                    rust-memoize-inner-0.2
                    rust-lazy-static-1
                    rust-anstream-0.6.18
                    rust-anstyle-1.0.11
                    rust-anstyle-parse-0.2.7
                    rust-anstyle-wincon-3.0.6
                    rust-is-terminal-polyfill-1.70.1
                    rust-anstyle-query-1.1.2
                    rust-colorchoice-1.0.3
                    rust-strsim-0.11.1
                    rust-utf8parse-0.2.2
                    rust-clap-lex-0.7.5
                    rust-heck-0.5.0
                    rust-proc-macro2-1.0.101
                    rust-quote-1.0.40
                    rust-syn-2.0.106
                    rust-bstr-1.12.0
                    rust-bytecount-0.6.9
                    rust-encoding-rs-0.8.35
                    rust-encoding-rs-io-0.1.7
                    rust-memmap2-0.9.8
                    rust-crossbeam-utils-0.8.21
                    rust-winapi-0.3.9
                    rust-syn-1.0.109
                    rust-bitflags-2.9.4
                    rust-bitflags-1.3.2
                    rust-cfg-if-1.0.0
                    rust-fastrand-2.2.0
                    rust-once-cell-1.20.2
                    rust-rustix-0.38.42
                    rust-windows-sys-0.59.0
                    rust-windows-targets-0.52.6
                    rust-hermit-abi-0.1.19
                    rust-hermit-abi-0.3.0
                    rust-libc-0.2.169
                    rust-linux-raw-sys-0.4.14
                    rust-errno-0.3.10
                    rust-log-0.4
                    rust-memchr-2.7.4
                    rust-regex-1
                    rust-regex-automata-0.4
                    rust-regex-syntax-0.8.5
                    rust-same-file-1.0
                    rust-tempfile-3
                    rust-termcolor-1
                    rust-unicode-ident-1.0.14
                    rust-unicode-width-0.1.14
                    rust-walkdir-2.4
                    rust-winapi-util-0.1.5
                    rust-base64-0.20.0
                    rust-serde-1.0.219
                    rust-serde-derive-1.0.219
                    rust-serde-json-1.0.140
                    rust-memmap2-0.5.10
                    rust-clap-complete-4.5
                    rust-clap-mangen-0.2
                    rust-roff-0.2.2
                    rust-textwrap-0.11.0
                    rust-itoa-1.0.15
                    rust-ryu-1.0.20
                    rust-winapi-i686-pc-windows-gnu-0.4.0
                    rust-winapi-x86-64-pc-windows-gnu-0.4.0
                    rust-windows-aarch64-gnullvm-0.52.6
                    rust-windows-x86-64-msvc-0.52.6
                    rust-windows-aarch64-msvc-0.52.6
                    rust-windows-i686-gnu-0.52.6
                    rust-windows-i686-gnullvm-0.52.6
                    rust-windows-i686-msvc-0.52.6
                    rust-windows-x86-64-gnu-0.52.6
                    rust-windows-x86-64-gnullvm-0.52.6))
  (voice-type =>
          (list rust-adler2-2.0.1
                rust-aho-corasick-1.1.4
                rust-alsa-0.9.1
                rust-alsa-sys-0.3.1
                rust-anstream-0.6.21
                rust-anstyle-1.0.13
                rust-anstyle-parse-0.2.7
                rust-anstyle-query-1.1.5
                rust-anstyle-wincon-3.0.11
                rust-anyhow-1.0.102
                rust-autocfg-1.5.0
                rust-base64-0.22.1
                rust-bindgen-0.72.1
                rust-bit-set-0.8.0
                rust-bit-vec-0.8.0
                rust-bitflags-1.3.2
                rust-bitflags-2.11.0
                rust-block-0.1.6
                rust-bumpalo-3.20.2
                rust-bytes-1.11.1
                rust-cairo-rs-0.20.12
                rust-cairo-sys-rs-0.20.10
                rust-cc-1.2.56
                rust-cesu8-1.1.0
                rust-cexpr-0.6.0
                rust-cfg-expr-0.20.6
                rust-cfg-if-1.0.4
                rust-clang-sys-1.8.1
                rust-clap-4.5.60
                rust-clap-builder-4.5.60
                rust-clap-derive-4.5.55
                rust-clap-lex-1.0.0
                rust-cocoa-0.22.0
                rust-colorchoice-1.0.4
                rust-combine-4.6.7
                rust-cookie-0.18.1
                rust-cookie-store-0.22.1
                rust-core-foundation-0.7.0
                rust-core-foundation-0.9.4
                rust-core-foundation-sys-0.7.0
                rust-core-foundation-sys-0.8.7
                rust-core-graphics-0.19.2
                rust-core-graphics-0.21.0
                rust-coreaudio-rs-0.11.3
                rust-coreaudio-sys-0.2.17
                rust-cpal-0.15.3
                rust-crc32fast-1.5.0
                rust-dasp-sample-0.11.0
                rust-deranged-0.5.6
                rust-displaydoc-0.2.5
                rust-document-features-0.2.12
                rust-either-1.15.0
                rust-equivalent-1.0.2
                rust-errno-0.3.14
                rust-fastrand-2.3.0
                rust-field-offset-0.3.6
                rust-find-msvc-tools-0.1.9
                rust-flate2-1.1.9
                rust-fnv-1.0.7
                rust-foldhash-0.1.5
                rust-foreign-types-0.3.2
                rust-foreign-types-shared-0.1.1
                rust-form-urlencoded-1.2.2
                rust-futures-channel-0.3.32
                rust-futures-core-0.3.32
                rust-futures-executor-0.3.32
                rust-futures-io-0.3.32
                rust-futures-macro-0.3.32
                rust-futures-task-0.3.32
                rust-futures-util-0.3.32
                rust-gdk-pixbuf-0.20.10
                rust-gdk-pixbuf-sys-0.20.10
                rust-gdk4-0.9.6
                rust-gdk4-sys-0.9.6
                rust-getrandom-0.2.17
                rust-getrandom-0.3.4
                rust-getrandom-0.4.1
                rust-gio-0.20.12
                rust-gio-sys-0.20.10
                rust-glib-0.20.12
                rust-glib-macros-0.20.12
                rust-glib-sys-0.20.10
                rust-glob-0.3.3
                rust-gobject-sys-0.20.10
                rust-graphene-rs-0.20.10
                rust-graphene-sys-0.20.10
                rust-gsk4-0.9.6
                rust-gsk4-sys-0.9.6
                rust-gtk4-0.9.7
                rust-gtk4-macros-0.9.5
                rust-gtk4-sys-0.9.6
                rust-hashbrown-0.15.5
                rust-hashbrown-0.16.1
                rust-heck-0.3.3
                rust-heck-0.5.0
                rust-hotkey-listener-0.3.2
                rust-hound-3.5.1
                rust-http-1.4.0
                rust-httparse-1.10.1
                rust-humantime-2.3.0
                rust-humantime-serde-1.1.1
                rust-icu-collections-2.1.1
                rust-icu-locale-core-2.1.1
                rust-icu-normalizer-2.1.1
                rust-icu-normalizer-data-2.1.1
                rust-icu-properties-2.1.2
                rust-icu-properties-data-2.1.2
                rust-icu-provider-2.1.1
                rust-id-arena-2.3.0
                rust-idna-1.1.0
                rust-idna-adapter-1.2.1
                rust-indexmap-2.13.0
                rust-is-terminal-polyfill-1.70.2
                rust-itertools-0.13.0
                rust-itertools-0.8.2
                rust-itoa-1.0.17
                rust-jni-0.21.1
                rust-jni-sys-0.3.0
                rust-jobserver-0.1.34
                rust-js-sys-0.3.85
                rust-lazy-static-1.5.0
                rust-leb128fmt-0.1.0
                rust-libc-0.2.182
                rust-libloading-0.8.9
                rust-linux-raw-sys-0.11.0
                rust-litemap-0.8.1
                rust-litrs-1.0.0
                rust-log-0.4.29
                rust-mach2-0.4.3
                rust-malloc-buf-0.0.6
                rust-matchers-0.2.0
                rust-memchr-2.8.0
                rust-memoffset-0.9.1
                rust-minimal-lexical-0.2.1
                rust-miniz-oxide-0.8.9
                rust-ndk-0.8.0
                rust-ndk-context-0.1.1
                rust-ndk-sys-0.5.0+25.2.9519653
                rust-nom-7.1.3
                rust-nu-ansi-term-0.50.3
                rust-num-conv-0.2.0
                rust-num-derive-0.4.2
                rust-num-enum-0.7.5
                rust-num-enum-derive-0.7.5
                rust-num-traits-0.2.19
                rust-objc-0.2.7
                rust-oboe-0.6.1
                rust-oboe-sys-0.6.1
                rust-once-cell-1.21.3
                rust-once-cell-polyfill-1.70.2
                rust-pango-0.20.12
                rust-pango-sys-0.20.10
                rust-percent-encoding-2.3.2
                rust-pin-project-lite-0.2.16
                rust-pkg-config-0.3.32
                rust-potential-utf-0.1.4
                rust-powerfmt-0.2.0
                rust-ppv-lite86-0.2.21
                rust-prettyplease-0.2.37
                rust-proc-macro-crate-3.4.0
                rust-proc-macro2-1.0.106
                rust-proptest-1.10.0
                rust-quick-error-1.2.3
                rust-quote-1.0.44
                rust-r-efi-5.3.0
                rust-rand-0.9.2
                rust-rand-chacha-0.9.0
                rust-rand-core-0.9.5
                rust-rand-xorshift-0.4.0
                rust-rdev-0.5.3
                rust-regex-1.12.3
                rust-regex-automata-0.4.14
                rust-regex-syntax-0.8.9
                rust-ring-0.17.14
                rust-rustc-hash-2.1.1
                rust-rustc-version-0.4.1
                rust-rustix-1.1.3
                rust-rustls-0.23.36
                rust-rustls-pki-types-1.14.0
                rust-rustls-webpki-0.103.9
                rust-rustversion-1.0.22
                rust-rusty-fork-0.3.1
                rust-ryu-1.0.23
                rust-same-file-1.0.6
                rust-secstr-0.5.1
                rust-semver-1.0.27
                rust-serde-1.0.228
                rust-serde-core-1.0.228
                rust-serde-derive-1.0.228
                rust-serde-json-1.0.149
                rust-serde-spanned-1.0.4
                rust-serde-yaml-0.9.34+deprecated
                rust-sharded-slab-0.1.7
                rust-shlex-1.3.0
                rust-simd-adler32-0.3.8
                rust-slab-0.4.12
                rust-smallvec-1.15.1
                rust-stable-deref-trait-1.2.1
                rust-strsim-0.11.1
                rust-structdoc-0.1.4
                rust-structdoc-derive-0.1.4
                rust-subtle-2.6.1
                rust-syn-1.0.109
                rust-syn-2.0.117
                rust-synstructure-0.13.2
                rust-system-deps-7.0.7
                rust-target-lexicon-0.13.3
                rust-tempfile-3.25.0
                rust-thiserror-1.0.69
                rust-thiserror-2.0.18
                rust-thiserror-impl-1.0.69
                rust-thiserror-impl-2.0.18
                rust-thread-local-1.1.9
                rust-time-0.3.47
                rust-time-core-0.1.8
                rust-time-macros-0.2.27
                rust-tinystr-0.8.2
                rust-toml-0.9.12+spec-1.1.0
                rust-toml-datetime-0.7.5+spec-1.1.0
                rust-toml-edit-0.23.10+spec-1.0.0
                rust-toml-parser-1.0.9+spec-1.1.0
                rust-toml-writer-1.0.6+spec-1.1.0
                rust-tracing-0.1.44
                rust-tracing-attributes-0.1.31
                rust-tracing-core-0.1.36
                rust-tracing-log-0.2.0
                rust-tracing-subscriber-0.3.22
                rust-unarray-0.1.4
                rust-unicode-ident-1.0.24
                rust-unicode-segmentation-1.12.0
                rust-unicode-xid-0.2.6
                rust-unindent-0.1.11
                rust-unsafe-libyaml-0.2.11
                rust-untrusted-0.9.0
                rust-ureq-3.2.0
                rust-ureq-proto-0.5.3
                rust-url-2.5.8
                rust-utf-8-0.7.6
                rust-utf8-iter-1.0.4
                rust-utf8parse-0.2.2
                rust-valuable-0.1.1
                rust-version-check-0.9.5
                rust-version-compare-0.2.1
                rust-wait-timeout-0.2.1
                rust-walkdir-2.5.0
                rust-wasi-0.11.1+wasi-snapshot-preview1
                rust-wasip2-1.0.2+wasi-0.2.9
                rust-wasip3-0.4.0+wasi-0.3.0-rc-2026-01-06
                rust-wasm-bindgen-0.2.108
                rust-wasm-bindgen-futures-0.4.58
                rust-wasm-bindgen-macro-0.2.108
                rust-wasm-bindgen-macro-support-0.2.108
                rust-wasm-bindgen-shared-0.2.108
                rust-wasm-encoder-0.244.0
                rust-wasm-metadata-0.244.0
                rust-wasmparser-0.244.0
                rust-web-sys-0.3.85
                rust-webpki-roots-1.0.6
                rust-winapi-0.3.9
                rust-winapi-i686-pc-windows-gnu-0.4.0
                rust-winapi-util-0.1.11
                rust-winapi-x86-64-pc-windows-gnu-0.4.0
                rust-windows-0.54.0
                rust-windows-aarch64-gnullvm-0.42.2
                rust-windows-aarch64-gnullvm-0.52.6
                rust-windows-aarch64-msvc-0.42.2
                rust-windows-aarch64-msvc-0.52.6
                rust-windows-core-0.54.0
                rust-windows-i686-gnu-0.42.2
                rust-windows-i686-gnu-0.52.6
                rust-windows-i686-gnullvm-0.52.6
                rust-windows-i686-msvc-0.42.2
                rust-windows-i686-msvc-0.52.6
                rust-windows-link-0.2.1
                rust-windows-result-0.1.2
                rust-windows-sys-0.45.0
                rust-windows-sys-0.52.0
                rust-windows-sys-0.59.0
                rust-windows-sys-0.61.2
                rust-windows-targets-0.42.2
                rust-windows-targets-0.52.6
                rust-windows-x86-64-gnu-0.42.2
                rust-windows-x86-64-gnu-0.52.6
                rust-windows-x86-64-gnullvm-0.42.2
                rust-windows-x86-64-gnullvm-0.52.6
                rust-windows-x86-64-msvc-0.42.2
                rust-windows-x86-64-msvc-0.52.6
                rust-winnow-0.7.14
                rust-wit-bindgen-0.51.0
                rust-wit-bindgen-core-0.51.0
                rust-wit-bindgen-rust-0.51.0
                rust-wit-bindgen-rust-macro-0.51.0
                rust-wit-component-0.244.0
                rust-wit-parser-0.244.0
                rust-writeable-0.6.2
                rust-x11-2.21.0
                rust-yoke-0.8.1
                rust-yoke-derive-0.8.1
                rust-zerocopy-0.8.39
                rust-zerocopy-derive-0.8.39
                rust-zerofrom-0.1.6
                rust-zerofrom-derive-0.1.6
                rust-zeroize-1.8.2
                rust-zerotrie-0.2.3
                rust-zerovec-0.11.5
                rust-zerovec-derive-0.11.2
                rust-zmij-1.0.21))
  (clio => (list rust-adler2-2.0.1
                 rust-ahash-0.8.12
                 rust-aho-corasick-1.1.4
                 rust-android-system-properties-0.1.5
                 rust-anstream-0.6.21
                 rust-anstyle-1.0.13
                 rust-anstyle-parse-0.2.7
                 rust-anstyle-query-1.1.5
                 rust-anstyle-wincon-3.0.11
                 rust-anyhow-1.0.102
                 rust-arboard-3.6.1
                 rust-arrayref-0.3.9
                 rust-arrayvec-0.7.6
                 rust-assert-cmd-2.1.2
                 rust-autocfg-1.5.0
                 rust-bitflags-2.11.0
                 rust-blake3-1.8.3
                 rust-block2-0.6.2
                 rust-bstr-1.12.1
                 rust-bumpalo-3.20.2
                 rust-bytemuck-1.25.0
                 rust-byteorder-lite-0.1.0
                 rust-cairo-rs-0.20.12
                 rust-cairo-sys-rs-0.20.10
                 rust-cc-1.2.56
                 rust-cfg-aliases-0.2.1
                 rust-cfg-expr-0.20.6
                 rust-cfg-if-1.0.4
                 rust-chrono-0.4.43
                 rust-clap-4.5.60
                 rust-clap-builder-4.5.60
                 rust-clap-derive-4.5.55
                 rust-clap-lex-1.0.0
                 rust-clipboard-win-5.4.1
                 rust-colorchoice-1.0.4
                 rust-constant-time-eq-0.4.2
                 rust-core-foundation-sys-0.8.7
                 rust-cpufeatures-0.2.17
                 rust-crc32fast-1.5.0
                 rust-crunchy-0.2.4
                 rust-ctrlc-3.5.1
                 rust-difflib-0.4.0
                 rust-directories-5.0.1
                 rust-dirs-sys-0.4.1
                 rust-dispatch2-0.3.0
                 rust-downcast-rs-1.2.1
                 rust-equivalent-1.0.2
                 rust-errno-0.3.14
                 rust-env-filter-1.0.0
                 rust-env-logger-0.11.9
                 rust-error-code-3.3.2
                 rust-fallible-iterator-0.3.0
                 rust-fallible-streaming-iterator-0.1.9
                 rust-fastrand-2.3.0
                 rust-fax-0.2.6
                 rust-fax-derive-0.2.0
                 rust-fdeflate-0.3.7
                 rust-field-offset-0.3.6
                 rust-find-msvc-tools-0.1.9
                 rust-fixedbitset-0.5.7
                 rust-flate2-1.1.9
                 rust-foldhash-0.1.5
                 rust-futures-channel-0.3.32
                 rust-futures-core-0.3.32
                 rust-futures-executor-0.3.32
                 rust-futures-io-0.3.32
                 rust-futures-macro-0.3.32
                 rust-futures-task-0.3.32
                 rust-futures-util-0.3.32
                 rust-gdk-pixbuf-0.20.10
                 rust-gdk-pixbuf-sys-0.20.10
                 rust-gdk4-0.9.6
                 rust-gdk4-sys-0.9.6
                 rust-gethostname-1.1.0
                 rust-getrandom-0.2.17
                 rust-getrandom-0.4.1
                 rust-gio-0.20.12
                 rust-gio-sys-0.20.10
                 rust-glib-0.20.12
                 rust-glib-macros-0.20.12
                 rust-glib-sys-0.20.10
                 rust-gobject-sys-0.20.10
                 rust-graphene-rs-0.20.10
                 rust-graphene-sys-0.20.10
                 rust-gsk4-0.9.6
                 rust-gsk4-sys-0.9.6
                 rust-gtk4-0.9.7
                 rust-gtk4-macros-0.9.5
                 rust-gtk4-sys-0.9.6
                 rust-half-2.7.1
                 rust-hashbrown-0.14.5
                 rust-hashbrown-0.15.5
                 rust-hashbrown-0.16.1
                 rust-hashlink-0.9.1
                 rust-heck-0.5.0
                 rust-humantime-2.3.0
                 rust-humantime-serde-1.1.1
                 rust-iana-time-zone-0.1.65
                 rust-iana-time-zone-haiku-0.1.2
                 rust-id-arena-2.3.0
                 rust-image-0.25.9
                 rust-indexmap-2.13.0
                 rust-is-terminal-polyfill-1.70.2
                 rust-itoa-1.0.17
                 rust-js-sys-0.3.87
                 rust-leb128fmt-0.1.0
                 rust-libc-0.2.182
                 rust-libredox-0.1.12
                 rust-libsqlite3-sys-0.30.1
                 rust-linux-raw-sys-0.11.0
                 rust-lock-api-0.4.14
                 rust-log-0.4.29
                 rust-memchr-2.8.0
                 rust-memoffset-0.9.1
                 rust-miniz-oxide-0.8.9
                 rust-moxcms-0.7.11
                 rust-nix-0.30.1
                 rust-nom-8.0.0
                 rust-num-traits-0.2.19
                 rust-objc2-0.6.3
                 rust-objc2-app-kit-0.3.2
                 rust-objc2-core-foundation-0.3.2
                 rust-objc2-core-graphics-0.3.2
                 rust-objc2-encode-4.1.0
                 rust-objc2-foundation-0.3.2
                 rust-objc2-io-surface-0.3.2
                 rust-once-cell-1.21.3
                 rust-once-cell-polyfill-1.70.2
                 rust-option-ext-0.2.0
                 rust-os-pipe-1.2.3
                 rust-pango-0.20.12
                 rust-pango-sys-0.20.10
                 rust-parking-lot-0.12.5
                 rust-parking-lot-core-0.9.12
                 rust-percent-encoding-2.3.2
                 rust-petgraph-0.8.3
                 rust-pin-project-lite-0.2.16
                 rust-pkg-config-0.3.32
                 rust-png-0.18.1
                 rust-predicates-3.1.4
                 rust-predicates-core-1.0.10
                 rust-predicates-tree-1.0.13
                 rust-prettyplease-0.2.37
                 rust-proc-macro-crate-3.4.0
                 rust-proc-macro2-1.0.106
                 rust-pxfm-0.1.27
                 rust-quick-error-2.0.1
                 rust-quick-xml-0.38.4
                 rust-quote-1.0.44
                 rust-r-efi-5.3.0
                 rust-redox-syscall-0.5.18
                 rust-redox-users-0.4.6
                 rust-regex-1.12.3
                 rust-regex-automata-0.4.14
                 rust-regex-syntax-0.8.9
                 rust-rusqlite-0.32.1
                 rust-rusqlite-migration-1.3.1
                 rust-rustc-version-0.4.1
                 rust-rustix-1.1.3
                 rust-rustversion-1.0.22
                 rust-ryu-1.0.23
                 rust-scopeguard-1.2.0
                 rust-semver-1.0.27
                 rust-serde-1.0.228
                 rust-serde-core-1.0.228
                 rust-serde-derive-1.0.228
                 rust-serde-json-1.0.149
                 rust-serde-spanned-1.0.4
                 rust-serde-yaml-0.9.34+deprecated
                 rust-shlex-1.3.0
                 rust-simd-adler32-0.3.8
                 rust-slab-0.4.12
                 rust-smallvec-1.15.1
                 rust-strsim-0.11.1
                 rust-syn-2.0.117
                 rust-system-deps-7.0.7
                 rust-target-lexicon-0.13.3
                 rust-tempfile-3.25.0
                 rust-termtree-0.5.1
                 rust-thiserror-1.0.69
                 rust-thiserror-2.0.18
                 rust-thiserror-impl-1.0.69
                 rust-thiserror-impl-2.0.18
                 rust-tiff-0.10.3
                 rust-toml-0.9.12+spec-1.1.0
                 rust-toml-datetime-0.7.5+spec-1.1.0
                 rust-toml-edit-0.23.10+spec-1.0.0
                 rust-toml-parser-1.0.9+spec-1.1.0
                 rust-toml-writer-1.0.6+spec-1.1.0
                 rust-tree-magic-mini-3.2.2
                 rust-unicode-ident-1.0.24
                 rust-unicode-xid-0.2.6
                 rust-unsafe-libyaml-0.2.11
                 rust-utf8parse-0.2.2
                 rust-vcpkg-0.2.15
                 rust-version-check-0.9.5
                 rust-version-compare-0.2.1
                 rust-wait-timeout-0.2.1
                 rust-wasi-0.11.1+wasi-snapshot-preview1
                 rust-wasip2-1.0.2+wasi-0.2.9
                 rust-wasip3-0.4.0+wasi-0.3.0-rc-2026-01-06
                 rust-wasm-bindgen-0.2.110
                 rust-wasm-bindgen-macro-0.2.110
                 rust-wasm-bindgen-macro-support-0.2.110
                 rust-wasm-bindgen-shared-0.2.110
                 rust-wasm-encoder-0.244.0
                 rust-wasm-metadata-0.244.0
                 rust-wasmparser-0.244.0
                 rust-wayland-backend-0.3.12
                 rust-wayland-client-0.31.12
                 rust-wayland-protocols-0.32.10
                 rust-wayland-protocols-wlr-0.3.10
                 rust-wayland-scanner-0.31.8
                 rust-wayland-sys-0.31.8
                 rust-weezl-0.1.12
                 rust-windows-aarch64-gnullvm-0.48.5
                 rust-windows-aarch64-gnullvm-0.52.6
                 rust-windows-aarch64-gnullvm-0.53.1
                 rust-windows-aarch64-msvc-0.48.5
                 rust-windows-aarch64-msvc-0.52.6
                 rust-windows-aarch64-msvc-0.53.1
                 rust-windows-core-0.62.2
                 rust-windows-i686-gnu-0.48.5
                 rust-windows-i686-gnu-0.52.6
                 rust-windows-i686-gnu-0.53.1
                 rust-windows-i686-gnullvm-0.52.6
                 rust-windows-i686-gnullvm-0.53.1
                 rust-windows-i686-msvc-0.48.5
                 rust-windows-i686-msvc-0.52.6
                 rust-windows-i686-msvc-0.53.1
                 rust-windows-implement-0.60.2
                 rust-windows-interface-0.59.3
                 rust-windows-link-0.2.1
                 rust-windows-result-0.4.1
                 rust-windows-strings-0.5.1
                 rust-windows-sys-0.48.0
                 rust-windows-sys-0.59.0
                 rust-windows-sys-0.60.2
                 rust-windows-sys-0.61.2
                 rust-windows-targets-0.48.5
                 rust-windows-targets-0.52.6
                 rust-windows-targets-0.53.5
                 rust-windows-x86-64-gnu-0.48.5
                 rust-windows-x86-64-gnu-0.52.6
                 rust-windows-x86-64-gnu-0.53.1
                 rust-windows-x86-64-gnullvm-0.48.5
                 rust-windows-x86-64-gnullvm-0.52.6
                 rust-windows-x86-64-gnullvm-0.53.1
                 rust-windows-x86-64-msvc-0.48.5
                 rust-windows-x86-64-msvc-0.52.6
                 rust-windows-x86-64-msvc-0.53.1
                 rust-winnow-0.7.14
                 rust-wit-bindgen-0.51.0
                 rust-wit-bindgen-core-0.51.0
                 rust-wit-bindgen-rust-0.51.0
                 rust-wit-bindgen-rust-macro-0.51.0
                 rust-wit-component-0.244.0
                 rust-wit-parser-0.244.0
                 rust-wl-clipboard-rs-0.9.3
                 rust-x11rb-0.13.2
                 rust-x11rb-protocol-0.13.2
                 rust-zerocopy-0.8.39
                 rust-zerocopy-derive-0.8.39
                 rust-zmij-1.0.21
                 rust-zune-core-0.4.12
                 rust-zune-jpeg-0.4.21))
  (ast-index =>
          (list rust-ahash-0.8.12
                rust-aho-corasick-1.1.4
                rust-android-system-properties-0.1.5
                rust-anes-0.1.6
                rust-anstream-0.6.21
                rust-anstyle-1.0.13
                rust-anstyle-parse-0.2.7
                rust-anstyle-query-1.1.5
                rust-anstyle-wincon-3.0.11
                rust-anyhow-1.0.100
                rust-autocfg-1.5.0
                rust-bitflags-1.3.2
                rust-bitflags-2.10.0
                rust-bstr-1.12.1
                rust-bumpalo-3.19.1
                rust-cast-0.3.0
                rust-cc-1.2.53
                rust-cfg-if-1.0.4
                rust-chrono-0.4.43
                rust-ciborium-0.2.2
                rust-ciborium-io-0.2.2
                rust-ciborium-ll-0.2.2
                rust-clap-4.5.54
                rust-clap-builder-4.5.54
                rust-clap-derive-4.5.49
                rust-clap-lex-0.7.7
                rust-colorchoice-1.0.4
                rust-colored-2.2.0
                rust-core-foundation-sys-0.8.7
                rust-criterion-0.5.1
                rust-criterion-plot-0.5.0
                rust-crossbeam-channel-0.5.15
                rust-crossbeam-deque-0.8.6
                rust-crossbeam-epoch-0.9.18
                rust-crossbeam-utils-0.8.21
                rust-crunchy-0.2.4
                rust-csv-1.4.0
                rust-csv-core-0.1.13
                rust-deranged-0.5.5
                rust-dirs-5.0.1
                rust-dirs-sys-0.4.1
                rust-displaydoc-0.2.5
                rust-either-1.15.0
                rust-encoding-rs-0.8.35
                rust-encoding-rs-io-0.1.7
                rust-equivalent-1.0.2
                rust-errno-0.3.14
                rust-fallible-iterator-0.3.0
                rust-fallible-streaming-iterator-0.1.9
                rust-fastrand-2.3.0
                rust-filetime-0.2.27
                rust-find-msvc-tools-0.1.8
                rust-form-urlencoded-1.2.2
                rust-fs2-0.4.3
                rust-fsevent-sys-4.1.0
                rust-getrandom-0.2.17
                rust-getrandom-0.3.4
                rust-globset-0.4.18
                rust-grep-matcher-0.1.8
                rust-grep-regex-0.1.14
                rust-grep-searcher-0.1.16
                rust-half-2.7.1
                rust-hashbrown-0.14.5
                rust-hashbrown-0.16.1
                rust-hashlink-0.9.1
                rust-heck-0.5.0
                rust-hermit-abi-0.5.2
                rust-iana-time-zone-0.1.64
                rust-iana-time-zone-haiku-0.1.2
                rust-icu-collections-2.1.1
                rust-icu-locale-core-2.1.1
                rust-icu-normalizer-2.1.1
                rust-icu-normalizer-data-2.1.1
                rust-icu-properties-2.1.2
                rust-icu-properties-data-2.1.2
                rust-icu-provider-2.1.1
                rust-idna-1.1.0
                rust-idna-adapter-1.2.1
                rust-ignore-0.4.25
                rust-indexmap-2.13.0
                rust-inotify-0.10.2
                rust-inotify-sys-0.1.5
                rust-instant-0.1.13
                rust-is-terminal-0.4.17
                rust-is-terminal-polyfill-1.70.2
                rust-itertools-0.10.5
                rust-itoa-1.0.17
                rust-js-sys-0.3.85
                rust-kqueue-1.1.1
                rust-kqueue-sys-1.0.4
                rust-lazy-static-1.5.0
                rust-libc-0.2.180
                rust-libredox-0.1.12
                rust-libsqlite3-sys-0.28.0
                rust-linux-raw-sys-0.11.0
                rust-litemap-0.8.1
                rust-lock-api-0.4.14
                rust-log-0.4.29
                rust-memchr-2.7.6
                rust-memmap2-0.9.9
                rust-mio-1.1.1
                rust-notify-7.0.0
                rust-notify-debouncer-mini-0.5.0
                rust-notify-types-1.0.1
                rust-num-conv-0.1.0
                rust-num-traits-0.2.19
                rust-once-cell-1.21.3
                rust-once-cell-polyfill-1.70.2
                rust-oorandom-11.1.5
                rust-option-ext-0.2.0
                rust-parking-lot-0.12.5
                rust-parking-lot-core-0.9.12
                rust-percent-encoding-2.3.2
                rust-pkg-config-0.3.32
                rust-plotters-0.3.7
                rust-plotters-backend-0.3.7
                rust-plotters-svg-0.3.7
                rust-potential-utf-0.1.4
                rust-powerfmt-0.2.0
                rust-proc-macro2-1.0.105
                rust-quote-1.0.43
                rust-r-efi-5.3.0
                rust-rayon-1.11.0
                rust-rayon-core-1.13.0
                rust-redox-syscall-0.5.18
                rust-redox-syscall-0.7.0
                rust-redox-users-0.4.6
                rust-regex-1.12.2
                rust-regex-automata-0.4.13
                rust-regex-syntax-0.8.8
                rust-rusqlite-0.31.0
                rust-rustix-1.1.3
                rust-rustversion-1.0.22
                rust-ryu-1.0.22
                rust-same-file-1.0.6
                rust-scopeguard-1.2.0
                rust-serde-1.0.228
                rust-serde-core-1.0.228
                rust-serde-derive-1.0.228
                rust-serde-json-1.0.149
                rust-shlex-1.3.0
                rust-smallvec-1.15.1
                rust-stable-deref-trait-1.2.1
                rust-streaming-iterator-0.1.9
                rust-strsim-0.11.1
                rust-syn-2.0.114
                rust-synstructure-0.13.2
                rust-tempfile-3.24.0
                rust-thiserror-1.0.69
                rust-thiserror-impl-1.0.69
                rust-time-0.3.45
                rust-time-core-0.1.7
                rust-time-macros-0.2.25
                rust-tinystr-0.8.2
                rust-tinytemplate-1.2.1
                rust-tree-sitter-0.19.5
                rust-tree-sitter-0.26.5
                rust-tree-sitter-bash-0.25.1
                rust-tree-sitter-c-sharp-0.23.1
                rust-tree-sitter-cpp-0.23.4
                rust-tree-sitter-dart-0.0.4
                rust-tree-sitter-elixir-0.3.4
                rust-tree-sitter-go-0.25.0
                rust-tree-sitter-groovy-0.1.2
                rust-tree-sitter-java-0.23.5
                rust-tree-sitter-kotlin-ng-1.1.0
                rust-tree-sitter-language-0.1.7
                rust-tree-sitter-lua-0.5.0
                rust-tree-sitter-objc-3.0.2
                rust-tree-sitter-php-0.24.2
                rust-tree-sitter-proto-0.4.0
                rust-tree-sitter-python-0.25.0
                rust-tree-sitter-r-1.2.0
                rust-tree-sitter-ruby-0.23.1
                rust-tree-sitter-rust-0.24.0
                rust-tree-sitter-scala-0.24.0
                rust-tree-sitter-sql-0.0.2
                rust-tree-sitter-swift-0.7.1
                rust-tree-sitter-typescript-0.23.2
                rust-unicode-ident-1.0.22
                rust-url-2.5.8
                rust-utf8-iter-1.0.4
                rust-utf8parse-0.2.2
                rust-uuid-1.19.0
                rust-vcpkg-0.2.15
                rust-version-check-0.9.5
                rust-walkdir-2.5.0
                rust-wasi-0.11.1+wasi-snapshot-preview1
                rust-wasip2-1.0.2+wasi-0.2.9
                rust-wasm-bindgen-0.2.108
                rust-wasm-bindgen-macro-0.2.108
                rust-wasm-bindgen-macro-support-0.2.108
                rust-wasm-bindgen-shared-0.2.108
                rust-web-sys-0.3.85
                rust-winapi-0.3.9
                rust-winapi-i686-pc-windows-gnu-0.4.0
                rust-winapi-util-0.1.11
                rust-winapi-x86-64-pc-windows-gnu-0.4.0
                rust-windows-aarch64-gnullvm-0.48.5
                rust-windows-aarch64-gnullvm-0.52.6
                rust-windows-aarch64-msvc-0.48.5
                rust-windows-aarch64-msvc-0.52.6
                rust-windows-core-0.62.2
                rust-windows-i686-gnu-0.48.5
                rust-windows-i686-gnu-0.52.6
                rust-windows-i686-gnullvm-0.52.6
                rust-windows-i686-msvc-0.48.5
                rust-windows-i686-msvc-0.52.6
                rust-windows-implement-0.60.2
                rust-windows-interface-0.59.3
                rust-windows-link-0.2.1
                rust-windows-result-0.4.1
                rust-windows-strings-0.5.1
                rust-windows-sys-0.48.0
                rust-windows-sys-0.52.0
                rust-windows-sys-0.59.0
                rust-windows-sys-0.61.2
                rust-windows-targets-0.48.5
                rust-windows-targets-0.52.6
                rust-windows-x86-64-gnu-0.48.5
                rust-windows-x86-64-gnu-0.52.6
                rust-windows-x86-64-gnullvm-0.48.5
                rust-windows-x86-64-gnullvm-0.52.6
                rust-windows-x86-64-msvc-0.48.5
                rust-windows-x86-64-msvc-0.52.6
                rust-wit-bindgen-0.51.0
                rust-writeable-0.6.2
                rust-yoke-0.8.1
                rust-yoke-derive-0.8.1
                rust-zerocopy-0.8.33
                rust-zerocopy-derive-0.8.33
                rust-zerofrom-0.1.6
                rust-zerofrom-derive-0.1.6
                rust-zerotrie-0.2.3
                rust-zerovec-0.11.5
                rust-zerovec-derive-0.11.2
                rust-zmij-1.0.16
                )))
