package de.z0rdak.yawp.api;

import de.z0rdak.yawp.api.events.flag.FlagCheckRequest;
import de.z0rdak.yawp.api.events.flag.FlagCheckResult;
import de.z0rdak.yawp.core.flag.FlagContext;
import de.z0rdak.yawp.core.flag.FlagState;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import de.z0rdak.yawp.core.region.RegionType;
import de.z0rdak.yawp.handler.HandlerUtil;
import de.z0rdak.yawp.handler.RegionIndex;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.core.BlockPos;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.level.Level;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import java.util.function.Consumer;
import java.util.function.Function;

public record FlagEvaluator(FlagCheckResult result) {

    public FlagState state() {
        return result.getFlagState();
    }

    public FlagEvaluator onAllow(Consumer<FlagCheckResult> action) {
        if (state() == FlagState.ALLOWED && action != null) action.accept(result);
        return this;
    }

    public FlagEvaluator onDeny(Consumer<FlagCheckResult> action) {
        if (state() == FlagState.DENIED && action != null) action.accept(result);
        return this;
    }

    public FlagEvaluator onDenyWithMsg(Consumer<FlagCheckResult> action) {
        var isPlayerFlag = FlagRegister.hasPlayerTag(this.result.getFlagCheck().getRegionFlag());
        if (state() == FlagState.DENIED && isPlayerFlag && action != null)
            action.andThen(MessageSender::sendFlagMsg).accept(result);
        return this;
    }

    public FlagEvaluator onDefault(Consumer<FlagCheckResult> action) {
        if ((state() == FlagState.DISABLED || state() == FlagState.UNDEFINED) && action != null) action.accept(result);
        return this;
    }

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
    public static FlagState processCheck(@NotNull FlagCheckRequest checkEvent, @Nullable Consumer<FlagCheckResult> onAllow, @Nullable Consumer<FlagCheckResult> onDeny) {
        FlagCheckResult result = evaluate(checkEvent);
        result = Services.FLAG_EVENT_DISPATCHER.post(result);
        var state = result.getFlagState();
        switch (state) {
            case ALLOWED -> { if (onAllow != null) onAllow.accept(result); }
            case DENIED  -> { if (onDeny  != null) onDeny.accept(result); }
        }
        return state;
    }

    public static FlagEvaluator process(@NotNull FlagCheckRequest checkEvent) {
        FlagCheckResult result = evaluate(checkEvent);
        result = Services.FLAG_EVENT_DISPATCHER.post(result);
        return new FlagEvaluator(result);
    }

    public static FlagState processCheckF(@NotNull FlagCheckRequest checkEvent, @Nullable Function<FlagCheckResult, FlagState> handleResult) {
        FlagCheckResult result = evaluate(checkEvent);
        result = Services.FLAG_EVENT_DISPATCHER.post(result);
        if (handleResult != null) return handleResult.apply(result);
        return result.getFlagState();
    }

    /**
     * Processes the given flag check event and executes the given consumer if the flag is denied.
     * <p>
     * This overload is equivalent to calling {@link #processCheck(FlagCheckRequest, Consumer, Consumer)}
     * with {@code onAllow} set to {@code null}.
     * </p>
     *
     * @param checkEvent the flag check event to process, must not be {@code null}
     * @param onDeny     the consumer to execute if the flag is denied
     * @return the resulting {@link FlagState} after processing
     * @see #processCheck(FlagCheckRequest, Consumer, Consumer)
     */
    public static FlagState processCheck(@NotNull FlagCheckRequest checkEvent, @Nullable Consumer<FlagCheckResult> onDeny) {
        return processCheck(checkEvent, null, onDeny);
    }

    /**
     * Processes the given flag check event with default behavior.
     * <p>
     * This overload is equivalent to calling {@link #processCheck(FlagCheckRequest, Consumer, Consumer)}
     * with both consumers set to {@code null}.
     * </p>
     *
     * @param checkEvent the flag check event to process, must not be {@code null}
     * @return the resulting {@link FlagState} after processing
     * @see #processCheck(FlagCheckRequest, Consumer, Consumer)
     */
    public static FlagState processCheck(@NotNull FlagCheckRequest checkEvent) {
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
    public static FlagCheckResult evaluate(FlagCheckRequest checkEvent) {
        var targetRegion = RegionIndex.findResponsibleRegion(checkEvent.getTarget(), checkEvent.getDimension());
        if (targetRegion == null) {
            return FlagCheckResult.Undefined(checkEvent);
        }
        var regionFlag = checkEvent.getRegionFlag();
        ;var flagContext = new FlagContext(targetRegion, regionFlag, targetRegion.getFlag(regionFlag.name()), checkEvent.getPlayer());
        var resultingContext = resolveFlag(targetRegion, flagContext);
        return new FlagCheckResult(checkEvent, resultingContext);
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

    public static void checkMobGrief(Entity entity, CallbackInfo ci) {
        checkMobGrief(entity.level(), entity.getOnPos(), ci);
    }

    public static void checkMobGrief(Entity entity, CallbackInfoReturnable<Boolean> cir) {
        checkMobGrief(entity.level(), entity.getOnPos(), cir);
    }

    public static void checkMobGrief(Level world, BlockPos pos, CallbackInfo ci) {
        if (HandlerUtil.isServerSide(world)) {
            FlagCheckRequest checkEvent = new FlagCheckRequest(pos, FlagRegister.MOB_GRIEFING, world.dimension());
            if (Services.FLAG_EVENT_DISPATCHER.post(checkEvent))
                return;
            processCheck(checkEvent, deny -> ci.cancel());
        }
    }

    public static void checkMobGrief(Level world, BlockPos pos, CallbackInfoReturnable<Boolean> cir) {
        if (HandlerUtil.isServerSide(world)) {
            FlagCheckRequest checkEvent = new FlagCheckRequest(pos, FlagRegister.MOB_GRIEFING, world.dimension());
            if (Services.FLAG_EVENT_DISPATCHER.post(checkEvent))
                return;

            processCheck(checkEvent, deny -> cir.setReturnValue(false));
        }
    }
}
