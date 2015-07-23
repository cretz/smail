## Integration Tests

### Normal Integration Tests

Standard integration tests that use things like the Java mail library can be run just as ``sbt it:test``.

### ImapTest Conformance Test

This project also includes a test server that can be executed to check conformance against Dovecot's
[ImapTest](http://imapwiki.org/ImapTest/). To get the IMAP test environment up and running, simply run ``vagrant up`` in
this folder. Then simply run the local server using ``sbt "it:runMain smail.imap.ImapTestServer"``. This will listen on
port 143. The VM with the IMAP tests can access this on ``192.168.50.1``. Then, when logged in to the VM (SSH
``localhost`` port ``2222`` with user/pass ``vagrant``/``vagrant``) and ``cd``'d into the ``~/imaptest-1.0.0`` folder,
it can be easily run like so:

    src/imaptest host=192.168.50.1 user=foo pass=bar test=src/tests/expunge

See more information about running IMAP tests at http://imapwiki.org/ImapTest/Running.