import React, { Component } from 'react';
import { Navbar, NavItem } from "../components/materialize"
import { Link } from "react-router-dom";

export default class NavBar extends Component {
  render() {
    return (
      <Navbar brand='logo' right>
        <NavItem><Link to="/">Scratch</Link></NavItem>
      </Navbar>
    );
  }
}
