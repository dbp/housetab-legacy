<apply template="base">

  <bind tag="above">
    <h2>Reset your password:</h2>
    <form method="post">
      <password-errors><error/> </password-errors>
      Password: <input type="password" name="password"><br>
      <confirm-errors><error/> </confirm-errors>
      Confirm: <input type="password" name="confirm">
      <input type="submit" value="Reset">
    </form>
  </bind>

</apply>