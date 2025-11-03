package de.z0rdak.yawp.platform.event;

import de.z0rdak.yawp.api.events.flag.FlagEvent;
import de.z0rdak.yawp.api.events.flag.FlagCheckRequest;
import de.z0rdak.yawp.api.events.flag.FlagCheckResult;
import de.z0rdak.yawp.platform.services.event.FlagEventDispatcher;

/**
 * Fabric-specific backend adapter for FlagEventDispatcher. Instantiated by the service loader.
 */

/**
 * Fabric-specific implementation of {@link FlagEventDispatcher}.
 * Propagates events to Fabric’s {@link FabricFlagEvents} system and the shared common bus.
 */
public final class FabricFlagEventDispatcher implements FlagEventDispatcher {

    @Override
    public <T extends FlagEvent> void post(T event) {
        if (event instanceof FlagEvent.Add add) {
            FabricFlagEvents.ADD_FLAG.invoker().add(add);
        } else if (event instanceof FlagEvent.Remove remove) {
            FabricFlagEvents.REMOVE_FLAG.invoker().remove(remove);
        } else if (event instanceof FlagEvent.UpdateFlagMessage update) {
            FabricFlagEvents.UPDATE_FLAG_MSG.invoker().updateMessage(update);
        }
    }

    @Override
    public boolean post(FlagCheckRequest event) {
        return FabricFlagEvents.CHECK_FLAG.invoker().checkFlag(event);
    }

    @Override
    public FlagCheckResult post(FlagCheckResult result) {
        return FabricFlagEvents.FLAG_RESULT.invoker().getResult(result);
    }
}
