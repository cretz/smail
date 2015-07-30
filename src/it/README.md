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

*Tests*

- [ ] append - Things are shown as recent or not across connections immediately
- [x] close
- [x] copy
- [ ] expunge - UID's reordered too quickly since deletes happen across connections immediately
- [ ] expunge2 - UID's reordered too quickly since deletes happen across connections immediately
- [x] fetch
- [x] fetch-body
- [x] fetch-envelope
- [x] list
- [ ] listext - Not handling now since it requires extra capability
- [x] search-addresses
- [x] search-body
- [ ] search-context-update - Not handling since it requires extra capability
- [ ] search-context-update2 - Not handling since it requires extra capability
- [x] search-date
- [x] search-flags
- [x] search-header
- [ ] search-sets - Need to be smarter about inclusion of last message and what not
- [ ] search-size - ???
- [ ] select - ???
- [ ] sort-addresses - ???
- [ ] sort-arrival - ???
- [ ] sort-date - ???
- [ ] sort-size - ???
- [ ] sort-subject - ???
- [ ] store - ???
- [ ] subscribe - ???
- [ ] thread - ???
- [ ] thread2 - ???
- [ ] thread3 - ???
- [ ] thread4 - ???
- [ ] thread5 - ???
- [ ] thread6 - ???
- [ ] thread7 - ???
- [ ] thread8 - ???