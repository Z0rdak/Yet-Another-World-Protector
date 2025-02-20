package de.z0rdak.yawp.api;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.api.events.region.FlagCheckResult;
import de.z0rdak.yawp.api.permission.Permissions;
import de.z0rdak.yawp.constants.Constants;
import de.z0rdak.yawp.core.flag.*;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.core.region.RegionType;
import de.z0rdak.yawp.data.region.RegionDataManager;
import de.z0rdak.yawp.handler.HandlerUtil;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.core.BlockPos;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.function.Consumer;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.core.flag.RegionFlag.BREAK_BLOCKS;
import static de.z0rdak.yawp.core.flag.RegionFlag.MOB_GRIEFING;

public class FlagEvaluator {

    /**
     * Processes a flag check event and executes the corresponding consumer based on the result.  
     * <p>
     * The given flag check event is evaluated to determine the flag state and then  
     * posted to the event bus for further processing. If the flag state is  
     * {@code ALLOWED}, the {@code onAllow} consumer  
     * is executed. If the flag state is {@code DENIED}, the {@code onDeny} consumer is executed.  
     * </p>
     *
     * @param checkEvent the flag check event to process, must not be {@code null}
     * @param onAllow    the consumer to execute if the flag is allowed, may be {@code null}
     * @param onDeny     the consumer to execute if the flag is denied, may be {@code null}
     * @return the resulting {@link FlagState} after processing the event
     */
    public static FlagState processCheck(@NotNull FlagCheckEvent checkEvent, @Nullable Consumer<FlagCheckResult> onAllow, @Nullable Consumer<FlagCheckResult> onDeny) {
        FlagCheckResult result = evaluate(checkEvent);
        result = Services.EVENT.post(result);
        var state = result.getFlagState();
        if (state == FlagState.ALLOWED && onAllow != null) {
            onAllow.accept(result);
        }
        if (state == FlagState.DENIED && onDeny != null) {
            onDeny.accept(result);
        }
        return state;
    }

    /**
     * Processes the given flag check event and executes the given consumer if the flag is denied.
     * <p>
     * This overload is equivalent to calling {@link #processCheck(FlagCheckEvent, Consumer, Consumer)}
     * with {@code onAllow} set to {@code null}.
     * </p>
     *
     * @param checkEvent the flag check event to process, must not be {@code null}
     * @param onDeny     the consumer to execute if the flag is denied
     * @return the resulting {@link FlagState} after processing
     * @see #processCheck(FlagCheckEvent, Consumer, Consumer)
     */
    public static FlagState processCheck(@NotNull FlagCheckEvent checkEvent, @Nullable Consumer<FlagCheckResult> onDeny) {
        return processCheck(checkEvent, null, onDeny);
    }

    /**
     * Processes the given flag check event with default behavior.
     * <p>
     * This overload is equivalent to calling {@link #processCheck(FlagCheckEvent, Consumer, Consumer)}
     * with both consumers set to {@code null}.
     * </p>
     *
     * @param checkEvent the flag check event to process, must not be {@code null}
     * @return the resulting {@link FlagState} after processing
     * @see #processCheck(FlagCheckEvent, Consumer, Consumer)
     */
    public static FlagState processCheck(@NotNull FlagCheckEvent checkEvent) {
        return processCheck(checkEvent, null, null);
    }

    /**
     * Evaluates a flag check event by determining the responsible region, resolving the flag context, 
     * and computing the resulting flag state.
     *
     * <p>The evaluation follows these steps:
     * <ul>
     *     <li>Finds the responsible region for the given target and dimension.</li>
     *     <li>If no region is found, returns an undefined {@link FlagCheckResult}.</li>
     *     <li>Constructs a {@link FlagContext} using the region, flag, and player information.</li>
     *     <li>Resolves the effective flag context by considering inheritance and overrides.</li>
     *     <li>Returns a {@link FlagCheckResult} containing the determined flag state and context details.</li>
     * </ul>
     *
     * @param checkEvent the flag check event containing information about the target, dimension, and flag.
     * @return a {@link FlagCheckResult} representing the evaluated flag state and context.
     */
    public static FlagCheckResult evaluate(FlagCheckEvent checkEvent) {
        var targetRegion = findResponsibleRegion(checkEvent.getTarget(), checkEvent.getDimension());
        if (targetRegion == null) {
            return FlagCheckResult.Undefined(checkEvent);
        }
        var regionFlag = checkEvent.getRegionFlag();
        var flagContext = new FlagContext(targetRegion, regionFlag, targetRegion.getFlag(regionFlag.name), checkEvent.getPlayer());
        var resultingContext = resolveFlag(targetRegion, flagContext);
        return new FlagCheckResult(checkEvent, resultingContext.resultingState(), resultingContext.region(), resultingContext.flag());
    }

    /**
     * Resolves the final {@link FlagContext} for a given region by considering flag inheritance 
     * from parent regions. The initial flag context needs to be the flag context corresponding to the given region.
     *
     * <p>This method follows a recursive approach to determine the effective flag context:</p>
     * <ul>
     *     <li>If the region is a root region, it returns the given {@code flagContext}.</li>
     *     <li>Otherwise, it retrieves the {@link FlagContext} of the parent region.</li>
     *     <li>The current flag context is merged with the parent's context using {@link FlagContext#inheritContext}.</li>
     *     <li>The process continues recursively up the region hierarchy until the root is reached.</li>
     * </ul>
     *
     * @param region      the region for which the flag context is being resolved.
     * @param flagContext the initial flag context for the region.
     * @return the resolved {@link FlagContext}, considering inheritance from parent regions.
     */
    public static FlagContext resolveFlag(IProtectedRegion region, FlagContext flagContext) {
        if (isRootRegion(region)) {
            return flagContext;
        }
        FlagContext parentFlagContext = FlagContext.parentOf(region, flagContext.regionFlag(), flagContext.player());
        FlagContext merged = flagContext.inheritContext(parentFlagContext);
        return resolveFlag(region.getParent(), merged);
    }

    /**
     * Checks whether the given region is its own parent. 
     * This indicates that the region is the global region.
     *
     * @param region the region to check, must not be {@code null}
     * @return {@code true} if the region's parent is itself, otherwise {@code false}
     */
    public static boolean isRootRegion(@NotNull IProtectedRegion region) {
        return region.equals(region.getParent()) && region.getRegionType() == RegionType.GLOBAL;
    }

    /**
     * Determines the region responsible for the given position and dimension.  
     * The responsible region is the one with the highest priority among all regions that cover the specified location.
     * <p>
     * If no specific region is found, this method falls back to the dimensional region.
     * If the dimensional region is inactive, it further falls back to the global region if it is active.
     * </p>
     *
     * @param pos the position for which to find the responsible region, must not be {@code null}
     * @param dim the dimension in which to search for the responsible region, must not be {@code null}
     * @return the highest-priority active region covering the given position and dimension,  
     *         or {@code null} if no active region is found
     */
    @Nullable
    public static IProtectedRegion findResponsibleRegion(@NotNull BlockPos pos, @NotNull ResourceKey<Level> dim) {
        var localRegion = getInvolvedRegionFor(pos, dim);
        if (localRegion == null) {
            var dimRegion = RegionDataManager.get().cacheFor(dim).getDimensionalRegion();
            if (dimRegion.isActive()) {
                return dimRegion;
            } else {
                var globalRegion = RegionDataManager.get().getGlobalRegion();
                return globalRegion.isActive() ? globalRegion : null;
            }
        }
        return localRegion;
    }

    /**
     * Gets all active regions which contain the provided position in the given dimension. <br>
     *
     * @param position the position to check for involved regions
     * @param dim      the dimension to check for involved regions
     * @return all active regions which contain the given location and dimension
     */
    private static List<IMarkableRegion> getInvolvedRegionsFor(BlockPos position, ResourceKey<Level> dim) {
        return RegionDataManager.get().getRegionsFor(dim).stream()
                .filter(IMarkableRegion::isActive)
                .filter(region -> region.contains(position))
                .collect(Collectors.toList());
    }

    /**
     * Gets the region with the highest priority among all involved regions at the given location and dimension. <br>
     * This considers the active state of the region as well. <br>
     *
     * @param position the position to check for involved regions
     * @param dim      the dimension to check for involved regions
     * @return the region with the highest priority among all involved regions which contain the given location
     */
    @Nullable
    private static IMarkableRegion getInvolvedRegionFor(BlockPos position, ResourceKey<Level> dim) {
        List<IMarkableRegion> regionsForPos = getInvolvedRegionsFor(position, dim);
        if (regionsForPos.isEmpty()) {
            return null;
        } else {
            return Collections.max(regionsForPos, Comparator.comparing(IMarkableRegion::getPriority));
        }
    }

    public static void checkMobGrief(Entity entity, CallbackInfo ci) {
        checkMobGrief(entity.level(), entity.getOnPos(), ci);
    }

    public static void checkMobGrief(Entity entity, CallbackInfoReturnable<Boolean> cir) {
        checkMobGrief(entity.level(), entity.getOnPos(), cir);
    }

    public static void checkMobGrief(Level world, BlockPos pos, CallbackInfo ci) {
        if (HandlerUtil.isServerSide(world)) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(pos, MOB_GRIEFING, world.dimension());
            if (Services.EVENT.post(checkEvent))
                return;
            processCheck(checkEvent, deny -> ci.cancel());
        }
    }

    public static void checkMobGrief(Level world, BlockPos pos, CallbackInfoReturnable<Boolean> cir) {
        if (HandlerUtil.isServerSide(world)) {
            FlagCheckEvent checkEvent = new FlagCheckEvent(pos, MOB_GRIEFING, world.dimension());
            if (Services.EVENT.post(checkEvent))
                return;
            processCheck(checkEvent, deny -> cir.setReturnValue(false));
        }
    }
}
