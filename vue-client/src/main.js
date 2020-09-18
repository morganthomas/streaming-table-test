import Vue from 'vue'
import App from './App.vue'

Vue.config.productionTip = false;

async function* getPeople() {
  const utf8Decoder = new TextDecoder('utf-8');
  const response = await fetch("http://localhost:8081/");
  const reader = response.body.getReader();
  let { value: chunk, done: readerDone } = await reader.read();
  chunk = chunk ? utf8Decoder.decode(chunk) : '';
  const re = /\n|\r|\r\n/gm;
  let startIndex = 0;
  let result;
  for (;;) {
    result = re.exec(chunk);
    if (!result) {
      if (readerDone) {
        break;
      }
      let remainder = chunk.substr(startIndex);
      ({ value: chunk, done: readerDone} = await reader.read());
      chunk = remainder + (chunk ? utf8Decoder.decode(chunk) : '');
      startIndex = re.lastIndex = 0;
      continue;
    }
    yield JSON.parse(chunk.substring(startIndex, result.index));
    startIndex = re.lastIndex;
  }
  if (startIndex < chunk.length) {
    yield JSON.parse(chunk.substr(startIndex));
  }
}

//const assumedRowHeight = 20;
//const assumedTableHeight = 500;

async function storePeople(people) {
  for await (let person of getPeople()) {
    people.push(person);
  }
}

const app = new Vue({
  render: h => h(App)
});

app.$mount('#app');

storePeople(app.$children[0].$data.people);
