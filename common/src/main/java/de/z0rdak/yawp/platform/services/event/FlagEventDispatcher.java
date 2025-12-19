package de.z0rdak.yawp.platform.services.event;

import de.z0rdak.yawp.api.events.flag.*;

public interface FlagEventDispatcher {

    <T extends FlagEvent> boolean post(T event);

    boolean post(FlagCheckRequest event);

    FlagCheckResult post(FlagCheckResult result);
}