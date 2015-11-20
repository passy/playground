const Rx = require('rx');
const deepcopy = require('deepcopy');

const API_DURATION_MS = 3000;
const DB_DURATION_MS = 1000;

const API = {
  fetchTvShow(id) {
    return Rx.Observable.create((observer) => {
      setTimeout(() => {
        observer.onNext({ title: "Mr. Robot", id: id, from: 'API' });
        observer.onCompleted();
      }, API_DURATION_MS);
    });
  }
};

const DB = {
  observable: new Rx.Subject(),

  fetchTvShow (id) {
    setTimeout(() => {
      this.observable.onNext({ title: "Mr. Robot", id: id, from: 'DB' });
    }, DB_DURATION_MS);
    return this.observable;
  },

  saveTvShow (show) {
    const showp = deepcopy(show);
    showp.from = 'DB\'';
    this.observable.onNext(showp);
  }
};

const main = () => {
  const oapi = API.fetchTvShow(1337).subscribe(DB.saveTvShow.bind(DB));
  const odb = DB.fetchTvShow(1337);
  odb.subscribe((x) => console.log('Value:', x));
};

main();

// Output:
// Value: { title: 'Mr. Robot', id: 1337, from: 'DB' }
// Value: { title: 'Mr. Robot', id: 1337, from: 'DB\'' }
