# Functional Requirements

## Session-Based Access

The service must generate a unique session link (similar to the LuckPerms online editor) when the YAWP mod connects.

## Region Data Integration

The YAWP mod sends region data to the service upon establishing a session.
The service provides an interface for viewing and modifying the data in real-time.

## Interactive Editing Interface

The interface should be web-based, interactive, and intuitive, offering:
A list of editable regions (CuboidRegion, SphereRegion) with specific attributes.

Ensure all edits are validated before they can be applied.

_Optional_: Drag-and-drop or point-and-click tools for adjusting region boundaries. A visual representation (e.g., grid or 3D view) to preview changes.

## Real-Time Synchronization

Changes made in the interface must sync with the mod in on command (e.g., a "Save" button).

## Temporary Persistence & Version Control

Store session data temporarily on the service while the session is active. Session is kept until actively closed or 15 mins have passed.
Once the session is closed, the data should be optionally retained for versioning.

Maintain a basic history of changes during the session to allow undo/redo functionality and offer a complete reset.

# Non-Functional Requirements

## Performance

The service should remain responsive even with multiple concurrent sessions.
Real-time synchronization delays must not exceed 200ms. Designed to handle a high number of sessions and users, scaling as demand increases. 
Session links should be secure and expire after a configurable time or when manually terminated. Data transmission must occur over HTTPS.

## Usability

The interface should be simple to navigate and similar in design to the LuckPerms editor for familiarity.
Include basic tooltips or a quick-start guide for new users.

## Metadata collection

Opt-In meta-data collection to give an overview on how the mod is used. Which flags are used, how are they used, etc.

## Data Synchronization

Synchronize efficiently, ensuring minimal disruption to the YAWP mod.
Provide a "Sync Now" option for manual updates in case of connectivity issues.

##  Localization

Support multiple languages for the user interface.