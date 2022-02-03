/* 
  To be able to run these tests it is necessary to install certain dependencies: 
      npm install cypress --save-dev 
   To run these tests use the command and select the test: 
      npm run test:integration
*/
context('User login', () => {
    describe('Accessing the login page', () => {
        it("Try to access the login page it should display an h1", () => {
            cy.visit("http://localhost:3000")
            cy.get("h1").contains("Pytherin Project")
            cy.get("h2").contains("To play you need to be logged in")
        })
    })

    describe('Login without fill fields', () => {
        it("Try to login without fill any field, shouldn't login", () => {
            cy.visit("http://localhost:3000")
            cy.get("input[type=submit]").contains("Login")
            .click()
            cy.get("#allEmptyValid").contains("* There are empty fields")
        })
    })

    describe('Login without fill email field', () => {
        it("Try to login without fill the email field, shouldn't login", () => {
            cy.visit("http://localhost:3000")
            cy.get("#inpsw").type("Heladera65")
            cy.get("input[type=submit]").contains("Login")
            .click()
            cy.get("#emailValid")
            .contains("* The input does not have a valid e-mail format")
        })
    })

    describe('Login without fill password field', () => {
        it("Try to login without fill the password field, shouldn't login", () => {
            cy.visit("http://localhost:3000")
            cy.get("#inemail").type("player0@example.com")
            cy.get("input[type=submit]").contains("Login")
            .click()
            cy.get("#allEmptyValid").contains("* There are empty fields")
        })
    })

    describe('Login with not registered email', () => {
        it("Try to login without fill the password field, shouldn't login", () => {
            cy.visit("http://localhost:3000")
            cy.get("#inemail").type("player99@email.com")
            cy.get("#inpsw").type("Heladera65")
            cy.get("input[type=submit]").contains("Login")
            .click()
            cy.get("#emailValid").contains("* Incorrect mail address")
        })
    })

    describe('Login with incorrect password', () => {
        it("Try to login without fill the password field, shouldn't login", () => {
            cy.visit("http://localhost:3000")
            cy.get("#inemail").type("player0@example.com")
            cy.get("#inpsw").type("Heladera66")
            cy.get("input[type=submit]").contains("Login")
            .click()
            cy.get("#pswValid").contains("* Incorrect password")
        })
    })

    describe('All OK', () => {
        it("Try to login with correct data. Should login", () => {
            cy.visit("http://localhost:3000")
            cy.get("#inemail").type("player0@example.com")
            cy.get("#inpsw").type("Heladera65")
            cy.get("input[type=submit]").contains("Login")
            .click()
            cy.get('h1').should("be.visible")
            cy.get('#create').should("be.visible").and("contain", "Create room")
            cy.get('#join').should("be.visible").and("contain", "Join room")
            cy.get('#change_nickname').should("be.visible").and("contain", "Change nickname")
            cy.get('#change_password').should("be.visible").and("contain", "Change password")
            cy.get('#logout').should("be.visible").and("contain", "Logout")
        })
    })



})