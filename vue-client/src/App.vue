<template>
  <div id="app">
      <table>
        <thead>
          <tr>
            <th>Name</th>
            <th>Age</th>
            <th>Sex</th>
            <th>Origin</th>
          </tr>
        </thead>
        <tbody>
          <tr v-for="(person,i) in visiblePeople" :key="i">
            <td>{{person.name}}</td>
            <td>{{person.age}}</td>
            <td>{{person.sex}}</td>
            <td>{{person.origin}}</td>
          </tr>
        </tbody>
      </table>
  </div>
</template>

<script>
export default {
  name: 'App',
  data: () => ({
    people: [],
    scrollY: 0
  }),
  computed: {
    visiblePeople: function () {
      return this.people.slice(0, Math.min(100, this.people.length));
    }
  }
}
</script>

<style>
#app {
  font-family: Avenir, Helvetica, Arial, sans-serif;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  text-align: center;
  color: #2c3e50;
  margin-top: 60px;
}
</style>
