import React from "react";
import { Route, Switch } from "react-router-dom";
import Login from "./pages/Login";

export default () =>
  <Switch>
    <Route path="/" exact component={Login} />
    <Route path="/login" exact component={Login} />
  </Switch>;
