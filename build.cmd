del /Q docs\*
copy static\* docs\
elm make src\Main.elm --optimize --output=docs\elm.js
npx uglifyjs docs\elm.js --compress "pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe" | npx uglifyjs --mangle --output docs\elm.min.js
del docs\elm.js
