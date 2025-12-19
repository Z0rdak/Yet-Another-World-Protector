package de.z0rdak.yawp.platform.event;

import de.z0rdak.yawp.api.events.flag.*;
import de.z0rdak.yawp.platform.services.event.FlagEventDispatcher;

/**
 * Fabric-specific implementation of {@link FlagEventDispatcher}.  Instantiated by the service loader.
 * Propagates events to Fabric’s {@link FabricFlagEvents} system and the shared common bus.
 */
public final class FabricFlagEventDispatcher implements FlagEventDispatcher {

    @Override
    public <T extends FlagEvent> boolean post(T event) {
        boolean canceled = false;
        if (event instanceof FlagEvent.Add e) {
            canceled = FabricFlagEvents.ADD_FLAG.invoker().add(e);
            e.setCanceled(canceled);
            FlagEvents.ON_ADD_FLAG.invoke(cb -> cb.add(e));
        } else if (event instanceof FlagEvent.Remove e) {
            canceled = FabricFlagEvents.REMOVE_FLAG.invoker().remove(e);
            e.setCanceled(canceled);
            FlagEvents.ON_REMOVE_FLAG.invoke(cb -> cb.remove(e));
        } else if (event instanceof FlagEvent.UpdateFlagMessage e) {
            FabricFlagEvents.UPDATE_FLAG_MSG.invoker().updateMessage(e);
            FlagEvents.ON_UPDATE_FLAG_MESSAGE.invoke(cb -> cb.updateMessage(e));
        }
        return canceled;
    }

    public boolean post(FlagCheckRequest event) {
        return FabricFlagEvents.CHECK_FLAG.invoker().checkFlag(event);
    }

    @Override
    public FlagCheckResult post(FlagCheckResult result) {
        return FabricFlagEvents.FLAG_RESULT.invoker().getResult(result);
    }
}
