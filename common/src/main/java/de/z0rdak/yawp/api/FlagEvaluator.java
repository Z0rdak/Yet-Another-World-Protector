package de.z0rdak.yawp.api;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.api.events.region.FlagCheckResult;
import de.z0rdak.yawp.api.permission.Permissions;
import de.z0rdak.yawp.constants.Constants;
import de.z0rdak.yawp.core.flag.FlagCorrelation;
import de.z0rdak.yawp.core.flag.FlagState;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.core.region.RegionType;
import de.z0rdak.yawp.data.region.RegionDataManager;
import de.z0rdak.yawp.core.flag.FlagContext;
import de.z0rdak.yawp.handler.HandlerUtil;
import de.z0rdak.yawp.core.flag.RegionFlagResolution;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.core.BlockPos;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;
import org.apache.commons.lang3.NotImplementedException;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.function.Consumer;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.core.flag.RegionFlag.MOB_GRIEFING;

public class FlagEvaluator {

    public static boolean playerHasBypassPermission(@NotNull IProtectedRegion region, @Nullable Player player) {
        return player != null && hasPermissionOrIsOpWithBypass(region, player);
    }

    private static boolean hasPermissionOrIsOpWithBypass(@NotNull IProtectedRegion region, @NotNull Player player) {
        boolean hasPermission = Permissions.get().hasAnyPermission(region, player, Permissions.getGroups(region, player));
        return hasPermission || Permissions.get().hasConfigPermAndOpBypassFlags(player);
    }

    /**
     * Processes a flag check event and executes the corresponding consumer based on the result.  
     * <p>
     * The given flag check event is evaluated to determine the flag state and then  
     * posted to the event bus for further processing. If the flag state is  
     * {@code ALLOWED}, {@code UNDEFINED}, or {@code DISABLED}, the {@code onAllow} consumer  
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
        if (result.getFlagState() == FlagState.ALLOWED && onAllow != null) {
            onAllow.accept(result);
        }
        if (result.getFlagState() == FlagState.DENIED && onDeny != null) {
            onDeny.accept(result);
        }
        return result.getFlagState();
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
     * Evaluates the given flag check event and determines the resulting flag state.  
     * <p>
     * This method identifies the responsible region for the given position and dimension,  
     * retrieves the applicable flag resolution from the region hierarchy,  
     * and returns the corresponding flag state as part of a {@link FlagCheckResult}.  
     * </p>
     * <p>
     * If no relevant region is found, the resulting flagstate defaults to {@link FlagState#UNDEFINED}.  
     * </p>
     *
     * @param checkEvent the flag check event to evaluate, must not be {@code null}
     * @return a {@link FlagCheckResult} containing the evaluated flag state, responsible region, and flag
     */
    public static FlagCheckResult evaluate(FlagCheckEvent checkEvent) {
        RegionFlag regionFlag = checkEvent.getRegionFlag();
        IProtectedRegion targetRegion = findResponsibleRegion(checkEvent.getTarget(), checkEvent.getDimension());
        if (targetRegion == null) {
            return FlagCheckResult.Undefined(checkEvent);
        }
        if (regionFlag == RegionFlag.BREAK_BLOCKS) {
            Constants.LOGGER.info("Breaking blocks");
        }
        RegionFlagResolution responsibleFlag = resolveFlag(targetRegion, regionFlag, checkEvent.getPlayer());
        // return new FlagCheckResult(checkEvent, responsibleFlag);
        throw new NotImplementedException();
    }

    public static RegionFlagResolution resolveFlag(IProtectedRegion region, RegionFlag regionFlag, @Nullable Player player) {
        //return getResponsibleFlag(new FlagContext(region, regionFlag, region.getFlag(regionFlag.name), player));
        throw new NotImplementedException();
    }

    //public static RegionFlagResolution getResponsibleFlag(FlagContext flagContext) {
    //    if (isRootRegion(flagContext.region())) {
    //        return flagContext.resultingFlag();
    //    }
    //    flagContext.checkAgainst(flagContext.region().getParent());
    //    return getResponsibleFlag(flagContext);
//
    //    // Check if the player has bypass permission on the current region
    //    if (playerHasBypassPermission(region, player)) {
    //        carry = new RegionFlagResolution(region, null);  // Player bypasses the region's flag
    //    } else {
    //        // Otherwise, check the flag at the current region
    //        IFlag flag = region.getFlag(regionFlag.name);
    //        boolean allowedOrDefined = region.getRegionFlags().isAllowedOrDefined(regionFlag.name);
    //        if (allowedOrDefined) {  // If the flag is allowed or defined, update the carry
    //            carry = new RegionFlagResolution(region, flag);
    //        } else {
    //            if (!allowedOrDefined) {
    //                carry = new RegionFlagResolution(region, null);
    //            }
    //        }
    //    }
//
    //    // Check the flag in the parent region
    //    IProtectedRegion parent = region.getParent();
    //    IFlag parentFlag = parent.getFlag(regionFlag.name);
    //    boolean parentFlagAllowedOrDefined = parent.getRegionFlags().isAllowedOrDefined(regionFlag.name);
    //    if (parentFlagAllowedOrDefined) {  // not null and not undefined 
    //        if (parentFlag.doesOverride()) {
    //            carry = new RegionFlagResolution(parent, parentFlag);
    //        } else // check if the players has bypass permission in the region, if so, the parent flag is not considered
    //            if (playerHasBypassPermission(region, player))
    //                carry = new RegionFlagResolution(region, null);
    //            else
    //                carry = new RegionFlagResolution(parent, parentFlag);
    //    }
    //    // recursive call to check the next parent
    //    return getResponsibleFlag(parent, regionFlag, player, carry);
    //}

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
    private static IProtectedRegion findResponsibleRegion(@NotNull BlockPos pos, @NotNull ResourceKey<Level> dim) {
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


    /**
     * Gets the flag state for the given region and flag. <br>
     * If the player is null, the flag state is returned as is. <br>
     * If the player is not null, the flag state is checked against the player's permissions. <br>
     * If the player is permitted, the flag state is returned as allowed. <br>
     *
     * @param region the region to get the flag state for
     * @param flag   the flag to get the state for
     * @param player the player to check the flag state against
     * @return the flag state for the given region and flag
     */
    private static FlagState getFlagState(IProtectedRegion region, RegionFlag flag, @Nullable Player player) {
        if (player == null) {
            return region.getFlags().flagState(flag.name);
        } else {
            boolean hasPermission = Permissions.get().hasAnyPermission(region, player, Permissions.getGroups(region, player));
            boolean isPermitted = hasPermission || Permissions.get().hasConfigPermAndOpBypassFlags(player);
            if (isPermitted) {
                return FlagState.ALLOWED;
            } else {
                return region.getFlags().flagState(flag.name);
            }
        }
    }
    
    /**
     * Gets the responsible region for the given position and dimension. <br>
     * The responsible region is the region with the highest priority among all involved regions at the given location and dimension. <br>
     *
     * @param pos the position to get the responsible region for
     * @param dim the dimension to get the responsible region for
     * @return the responsible region for the given position and dimension
     */
    @Nullable
    private static IProtectedRegion getResponsible(BlockPos pos, ResourceKey<Level> dim) {
        IMarkableRegion region = getInvolvedRegionFor(pos, dim);
        if (region == null) {
            IProtectedRegion dimRegion = RegionDataManager.get().cacheFor(dim).getDimensionalRegion();
            if (dimRegion.isActive()) {
                return dimRegion;
            } else {
                return RegionDataManager.get().getGlobalRegion().isActive()
                        ? RegionDataManager.get().getGlobalRegion()
                        : null;
            }
        }
        return region;
    }

    public static FlagCorrelation getResponsibleFlag(IProtectedRegion region, RegionFlag regionFlag, @Nullable FlagCorrelation carry) {
        if (carry == null) {
            FlagState flagState = region.getFlags().flagState(regionFlag.name);
            if (flagState == FlagState.ALLOWED || flagState == FlagState.DENIED) {
                IFlag flag = region.getFlag(regionFlag.name);
                carry = new FlagCorrelation(region, flag);
            } else
                carry = new FlagCorrelation(region, null);
        }
        if (region.equals(region.getParent())) {
            if (region.getFlags().flagState(regionFlag.name) != FlagState.UNDEFINED) {
                if (carry.getFlag() == null) {
                    carry = new FlagCorrelation(region, region.getFlag(regionFlag.name));
                }
            }
            return carry;
        }
        FlagState flagState = region.getParent().getFlags().flagState(regionFlag.name);
        if (flagState == FlagState.ALLOWED || flagState == FlagState.DENIED) {
            IFlag flag = region.getParent().getFlag(regionFlag.name);
            if (carry.getFlag() == null) {
                carry = new FlagCorrelation(region.getParent(), flag);
            }
            if (flag.doesOverride()) {
                carry = new FlagCorrelation(region.getParent(), flag);
            }
        }
        return getResponsibleFlag(region.getParent(), regionFlag, carry);
    }
}
