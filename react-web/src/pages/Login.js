import React, { Component } from "react";

import {TextInput, CardPanel, Input, Row, Button} from "../components/materialize"

export default class Login extends Component {
  constructor(props) {
    super(props);

    this.state = {
      email: "",
      password: ""
    };
  }

  validateForm() {
    return this.state.email.length > 0 && this.state.password.length > 0;
  }

  handleChange = event => {
    this.setState({
      [event.target.id]: event.target.value
    });
  }

  handleSubmit = event => {
    event.preventDefault();
  }

  render() {
    return (
      <div className="container">
        <h5>Log in</h5>
        <CardPanel>
          <form onSubmit={this.handleSubmit}>
            <Row><TextInput email label="Email" noLayout/></Row>
            <Row><TextInput password label="Password" noLayout/></Row>
            <p>By using this tool you agree to four things.</p>
            <ol>
              <li>You understand what "Browser Cookies" are.</li>
              <li>You understand what "Use at your own risk." means.</li>
              <li>You understand what "We are not liable for any and all damages cause by use of this tool." means.</li>
              <li>This tool uses Browser Cookies and you will use this tool at your own risk and we are not liable for any and all damages caused by using this tool.</li>
            </ol>
            <div className="center-align">
              <Button waves="light" disabled={!this.validateForm()}>Log In</Button>
            </div>
            <div className="center-align">
              <a href="javascript:void(0);">I forgot my password.</a>
            </div>
            <div class="center-align row">
              <a href="javascript:void(0);">Create a free account.</a>
            </div>
          </form>
        </CardPanel>
      </div>
    );
  }
}
