## mod_apns
 An ejabberd module to send offline messages as PUSH notifications for iOS.

**IMPORTANT: Fork originally made to change the database from Mnesia to ODBC.**

### Table of Contents
1. [Requirements](#requirements)
2. [Compilation](#compilation)
3. [Configuration](#configuration)
4. [Usage](#usage)

### Requirements

Module should be working fine with Ejabberd 14+.

#### Dependencies

This module uses other modules for some of its features.

1. *mod_muc \& mod_muc_rom:* to handle muc offline messages
2. *mod_offline:* to handle the badge number

#### Database

This module needs an ODBC configuration and the next table on the database:

> apns_users

|   Field   | Type         | Null | Default |
|:---------:|--------------|------|---------|
| user      | varchar(200) |      |         |
| token     | varchar(500) |      |         |
| last_seen | int(11)      |      |         |
| notification_group_enabled | tinyint(4)      |  no    | 1        |
| notification_enabled | tinyint(4)      |  no    | 1        |
| vibration_group_enabled | tinyint(4)      | no     |  1       |
| vibration_enabled | tinyint(4)      | no     |  1       |
| sound_type | varchar(200)      |  no  | 'default'  |

### Compilation

Because of the dependencies such as xml.hrl, logger.hrl, etc it's recommended to compile the module with ejabberd itself: put it in the *ejabberd/src* directory and run the default compiler.

### Configuration

To let the module work fine with Apple Push Notification Service APIs, put these lines in the modules section:

```yaml
mod_apns:
  address: "gateway.push.apple.com"
  port: 2195
  certfile: "cert.pem"
  keyfile: "key.pem"
```
You can use a *password* field in case if you have a password-protected certificate.

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
push_new_message
```
