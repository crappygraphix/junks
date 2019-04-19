import React, { Component } from 'react';
import NavBar from "../components/NavBar"
import Routes from "../Routes";

export default class App extends Component {
  render() {
    return (
      <div>
        <NavBar/>
        <Routes/>
      </div>
    );
  }
}
