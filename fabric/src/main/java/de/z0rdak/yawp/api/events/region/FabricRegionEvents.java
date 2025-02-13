package de.z0rdak.yawp.api.events.region;

import net.fabricmc.fabric.api.event.Event;
import net.fabricmc.fabric.api.event.EventFactory;

/**
 * Events about region and flag checks
 *
 * <p>These events can be categorized into three groups:
 * <ol>
 * <li>Simple listeners: ... </li>
 * <li>Predicates: {@link #CREATE_REGION}, {@link #DELETE_REGION}, {@link #RENAME_REGION}, {@link #UPDATE_AREA}, {@link #CHECK_FLAG} </li>
 * <li>Modifiers: {@link #FLAG_RESULT} </li>
 * </ol>
 */
public final class FabricRegionEvents {

    public static final Event<CreateRegion> CREATE_REGION = EventFactory.createArrayBacked(CreateRegion.class, callbacks -> (createRegionEvent) -> {
        for (CreateRegion callback : callbacks) {
            if (!callback.createRegion(createRegionEvent)) {
                return true;
            }
        }
        return false;
    });
    public static final Event<RemoveRegion> DELETE_REGION = EventFactory.createArrayBacked(RemoveRegion.class, callbacks -> (removeRegionEvent) -> {
        for (RemoveRegion callback : callbacks) {
            if (!callback.deleteRegion(removeRegionEvent)) {
                return true;
            }
        }
        return false;
    });
    public static final Event<RenameRegion> RENAME_REGION = EventFactory.createArrayBacked(RenameRegion.class, callbacks -> (renameRegionEvent) -> {
        for (RenameRegion callback : callbacks) {
            if (!callback.renameRegion(renameRegionEvent)) {
                return true;
            }
        }
        return false;
    });

    public static final Event<UpdateArea> UPDATE_AREA = EventFactory.createArrayBacked(UpdateArea.class, callbacks -> (updateAreaEvent) -> {
        for (UpdateArea callback : callbacks) {
            if (!callback.updateArea(updateAreaEvent)) {
                return true;
            }
        }
        return false;
    });
    /**
     * This event is only called at the server-side.
     */
    public static final Event<CheckFlag> CHECK_FLAG = EventFactory.createArrayBacked(CheckFlag.class, callbacks -> (flagCheckEvent) -> {
        for (CheckFlag callback : callbacks) {
            if (!callback.checkFlag(flagCheckEvent)) {
                return true;
            }
        }
        return false;
    });
    /**
     * This event is only called at the server-side.
     */
    public static final Event<FlagResult> FLAG_RESULT = EventFactory.createArrayBacked(FlagResult.class, callbacks -> (flagCheckResult) -> {
        for (FlagResult callback : callbacks) {
            flagCheckResult.setFlagState(callback.getResult(flagCheckResult).getFlagState());
        }
        return flagCheckResult;
    });

    private FabricRegionEvents() {
    }

    @FunctionalInterface
    public interface CreateRegion {
        boolean createRegion(RegionEvent.Create createRegionEvent);
    }

    @FunctionalInterface
    public interface RemoveRegion {
        boolean deleteRegion(RegionEvent.Remove removeRegionEvent);
    }

    @FunctionalInterface
    public interface RenameRegion {
        boolean renameRegion(RegionEvent.Rename renameRegionEvent);
    }

    @FunctionalInterface
    public interface UpdateArea {
        boolean updateArea(RegionEvent.UpdateArea updateAreaEvent);
    }

    @FunctionalInterface
    public interface FlagResult {
        FlagCheckResult getResult(FlagCheckResult flagCheckResult);
    }

    @FunctionalInterface
    public interface CheckFlag {
        boolean checkFlag(FlagCheckEvent flagCheckEvent);
    }
}