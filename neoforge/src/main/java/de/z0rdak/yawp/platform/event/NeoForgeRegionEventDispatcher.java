package de.z0rdak.yawp.platform.event;

import de.z0rdak.yawp.api.events.region.RegionEvent;
import de.z0rdak.yawp.platform.services.event.RegionEventDispatcher;
import net.neoforged.neoforge.common.NeoForge;

/**
 * Forge-specific backend for {@link RegionEventDispatcher}.
 * Integrates forge event bus with the common region event system.
 */
public final class NeoForgeRegionEventDispatcher implements RegionEventDispatcher {

    @Override
    public <T extends RegionEvent> boolean post(T event) {
        if (event instanceof RegionEvent.Create create) {
            NeoForgeRegionEvent.Create forgeEvent = new NeoForgeRegionEvent.Create(create);
            NeoForge.EVENT_BUS.post(forgeEvent);
            return forgeEvent.isCanceled() || create.isCanceled();
        }
        else if (event instanceof RegionEvent.Rename rename) {
            NeoForgeRegionEvent.Rename forgeEvent = new NeoForgeRegionEvent.Rename(rename);
            NeoForge.EVENT_BUS.post(forgeEvent);
            return forgeEvent.isCanceled() || rename.isCanceled();
        }
        else if (event instanceof RegionEvent.UpdateArea updateArea) {
            NeoForgeRegionEvent.UpdateArea forgeEvent = new NeoForgeRegionEvent.UpdateArea(updateArea);
            NeoForge.EVENT_BUS.post(forgeEvent);
            return forgeEvent.isCanceled() || updateArea.isCanceled();
        }
        else if (event instanceof RegionEvent.Remove remove) {
            NeoForgeRegionEvent.Remove forgeEvent = new NeoForgeRegionEvent.Remove(remove);
            NeoForge.EVENT_BUS.post(forgeEvent);
            return forgeEvent.isCanceled() || remove.isCanceled();
        }
        else if (event instanceof RegionEvent.PlayerEnter enter) {
            NeoForgeRegionEvent.PlayerEnter forgeEvent = new NeoForgeRegionEvent.PlayerEnter(
                    enter.getRegion(), enter.getPlayer(), enter.previous(), enter.current()
            );
            return NeoForge.EVENT_BUS.post(forgeEvent).isCanceled() || enter.isCanceled();
        }
        else if (event instanceof RegionEvent.PlayerLeave leave) {
            NeoForgeRegionEvent.PlayerLeave forgeEvent = new NeoForgeRegionEvent.PlayerLeave(
                    leave.getRegion(), leave.getPlayer(), leave.previous(), leave.current()
            );
            return NeoForge.EVENT_BUS.post(forgeEvent).isCanceled() || leave.isCanceled();
        }
        return false;
    }

}
