package de.z0rdak.yawp.platform.services.event;

import de.z0rdak.yawp.api.events.region.RegionEvent;

public interface RegionEventDispatcher {

    <T extends RegionEvent> boolean post(T event);
}
