<apply template="base">

  <bind tag="above">
    <form method="post">
      <name-errors><error/> </name-errors>
      Name: <input type="text" name="name"><br>
      <email-errors><error/> </email-errors>
      Email: <input type="text" name="email"><br>
      <password-errors><error/> </password-errors>
      Password: <input type="password" name="password"><br>
      <confirm-errors><error/> </confirm-errors>
      Confirm: <input type="confirm" name="confirm">
      <input type="submit" value="Signup">
    </form>
  </bind>
  
</apply>