package de.z0rdak.yawp.core.flag;

import de.z0rdak.yawp.api.Flag;
import de.z0rdak.yawp.api.FlagRegister;
import de.z0rdak.yawp.api.permission.Permissions;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import net.minecraft.world.entity.player.Player;
import org.jetbrains.annotations.Nullable;

/**
 * Represents the context for evaluating a flag within a protected region.
 *
 * <p>This record stores the region being checked, the specific flag under evaluation, 
 * an optional pre-determined flag value, and the player (if applicable) for permission checks.</p>
 *
 * @param region     The protected region where the flag is being evaluated.
 * @param regionFlag The flag being checked.
 * @param flag       The resolved flag value, if already determined (nullable).
 * @param player     The player for whom the flag is being evaluated (nullable).
 */
public record FlagContext(
        IProtectedRegion region, 
        Flag regionFlag,
        @Nullable IFlag flag, 
        @Nullable Player player) {

    /**
     * Resolves the effective {@link FlagState} for this context, accounting for whether the flag
     * is player-specific, beneficial, and whether the player has a bypass permission.
     *
     * <p>The resolution logic is as follows:</p>
     * <ul>
     *   <li>If the flag is {@code null}, the result is {@link FlagState#UNDEFINED}.</li>
     *   <li>If the flag is <strong>not</strong> player-specific, the result is taken directly
     *       from the region’s flag configuration via {@link RegionFlags#flagState(String)}.</li>
     *   <li>If the flag <strong>is</strong> player-specific:
     *     <ul>
     *       <li>If the flag is beneficial and the player has a bypass permission:
     *         <ul>
     *           <li>If the region’s flag state is {@link FlagState#ALLOWED} or {@link FlagState#DENIED},
     *               the result is {@link FlagState#ALLOWED}.</li>
     *           <li>Otherwise, the result is {@link FlagState#DISABLED}.</li>
     *         </ul>
     *       </li>
     *       <li>If the flag is beneficial and the player lacks a bypass permission,
     *           the result is the region’s defined {@link FlagState}.</li>
     *       <li>If the flag is non-beneficial and the player has a bypass permission,
     *           the result is {@link FlagState#DISABLED}.</li>
     *       <li>If the flag is non-beneficial and the player lacks a bypass permission,
     *           the result is the region’s defined {@link FlagState}.</li>
     *     </ul>
     *   </li>
     * </ul>
     *
     * <p>This method ensures that bypass permissions never result in a denied outcome,
     * and that beneficial flags are treated permissively for players with bypass privileges.</p>
     *
     * @return the resolved {@link FlagState}, reflecting region configuration,
     *         flag characteristics, and player permissions
     */
    public FlagState resultingState() {
        if (flag == null) {
            return FlagState.UNDEFINED;
        }
        boolean playerPerm = Permissions.playerHasBypassPermission(region, player);
        var flagState = region.getFlags().flagState(regionFlag.name());
        if (!FlagRegister.hasPlayerTag(regionFlag)) {
            return flagState;
        }
        if (FlagRegister.isBeneficial(regionFlag)) {
            if (playerPerm) {
                return switch (flagState) {
                    case ALLOWED, DENIED -> FlagState.ALLOWED;
                    default -> FlagState.DISABLED;
                };
            }
            return flagState;
        } else
            return playerPerm ? FlagState.DISABLED : flagState;
    }

    /**
     * Determines which {@link FlagContext} should take effect by applying inheritance logic
     * between this context and a given parent context.
     *
     * <p>The resolution follows these rules:</p>
     * <ul>
     *   <li>If the parent context’s flag exists, is set, and is marked as overriding
     *       ({@code flag.doesOverride()}), the parent context takes precedence.</li>
     *   <li>If the parent’s flag is set but the child’s flag is not set, the parent context takes precedence.</li>
     *   <li>In all other cases (including when neither flag is set), the current context is retained.</li>
     * </ul>
     *
     * <p>This method is used during recursive flag resolution to ensure that inherited or
     * overriding flag values from parent regions take effect only when explicitly allowed.</p>
     *
     * @param parent the parent {@link FlagContext} to compare against, must not be {@code null}
     * @return the effective {@link FlagContext} after applying inheritance rules
     */
    public FlagContext inheritContext(FlagContext parent) {
        boolean childFlagSet = region.getFlags().isSet(regionFlag.name());
        boolean parentFlagSet = parent.region.getFlags().isSet(regionFlag.name());
        boolean parentOverrides = parent.flag != null && parentFlagSet && parent.flag.doesOverride();
        boolean parentSetButNotChild = parentFlagSet && !childFlagSet;
        return (parentOverrides || parentSetButNotChild)
                ? parent
                : this; // what about none set? -> keep current
    }

    /**
     * Creates a {@link FlagContext} for the parent region of the given region.
     * <p>
     * This method retrieves the parent region of the specified {@code region} and constructs  
     * a {@link FlagContext} using the parent's flag value for the given {@link Flag}.
     * </p>
     *
     * @param region     the region whose parent context is to be determined, must not be {@code null}
     * @param regionFlag the flag for which the context is being created, must not be {@code null}
     * @param player     the player associated with this flag context, may be {@code null}
     * @return a {@link FlagContext} representing the flag state in the parent region
     */
    public static FlagContext parentOf(IProtectedRegion region, Flag regionFlag, @Nullable Player player) {
        IProtectedRegion parent = region.getParent();
        IFlag flag = parent.getFlag(regionFlag.name());
        return new FlagContext(parent, regionFlag, flag, player);
    }
}
    
