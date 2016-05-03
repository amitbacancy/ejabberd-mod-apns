## mod_apns
 An ejabberd module to send offline messages as PUSH notifications for iOS.

### Table of Contents
1. [Requirements](#requirements)
2. [Compilation](#compilation)
3. [Configuration](#configuration)
4. [Usage](#usage)

### Requirements

This module should be working well with Ejabberd 14+.

However this module uses the *FastXML* library, which was included since version 16.02.

If you want to use it with older versions, you have two options:

1. Add the *FastXML* library as a dependency of your version.
2. Replace all the *fxml* calls to *xml* (from *p1_xml* library).

#### Dependencies

This module uses other modules for some of its features.

1. *mod_muc \& mod_muc_rom:* to handle muc offline messages
2. *mod_offline:* to handle the badge number
 
The FastXML library mentioned above is also a dependency.

#### Database

This module needs an ODBC configuration and some tables on the database:

+ **apns_users**, which contains the token and the push settings for each user
+ **muc**, which contains the name of each room
+ **vcard_search**, which contains the full name (fn) of each user
+ **much\_push\_silence**, which contains the silence settings for each user and room

> apns_users

|   Field   | Type         | Null | Default |
|:---------:|--------------|------|---------|
| user      | varchar |      |         |
| token     | varchar |      |         |
| last_seen | int(11)      |      |         |
| notification_group_enabled | tinyint(4)      |  no    | 1        |
| notification_enabled | tinyint(4)      |  no    | 1        |
| vibration_group_enabled | tinyint(4)      | no     |  1       |
| vibration_enabled | tinyint(4)      | no     |  1       |
| sound_type | varchar      |  no  | 'default'  |

> muc\_push\_silence

|   Field   | Type    | Null | 
|:---------:|---------|------|
| muc_jid   | varchar |  no  |
| user_jid  | varchar |  no  |

The **muc** and **vcard_search** tables exists by default on Ejabberd.

### Compilation

Because of the dependencies, it's recommended to compile the module with ejabberd itself: put it in the *ejabberd/src* directory and run the default compiler. For example with the Make tool:

> make install

### Configuration

To let the module work fine with Apple Push Notification Service APIs, put these lines in the modules section:

```yaml
mod_apns:
  address: "gateway.push.apple.com"
  port: 2195
  certfile: "cert.pem"
```
You can use a password field in case you have a password-protected certificate.

Also there are available the same variables for the sandbox environment with the 'sbox-' prefix. 

For example the 'certfile' for the sandbox environment should be defined as 'sboxcertfile'.

### Usage

#### Client to server

You need to send this stanza to the server over the XMPP connection, to let the server know your client token:
```xml
<iq to="YourServer" type="set">
  <register xmlns="https://apple.com/push" >
    <token>TOKEN</token>
  </register>
</iq>
```
#### Server to APNS

It works with two *loc-keys* and one *log-args*:

```
push_new_message
push_new_muc_message
```

The idea is to be able to show different push messages on simple messages and on muc messages.
The expected parameter is the name of the sender or the name of the muc room.
