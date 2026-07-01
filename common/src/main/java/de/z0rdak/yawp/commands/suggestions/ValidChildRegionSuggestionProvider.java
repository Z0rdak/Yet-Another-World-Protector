package de.z0rdak.yawp.commands.suggestions;

import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.suggestion.SuggestionProvider;
import com.mojang.brigadier.suggestion.Suggestions;
import com.mojang.brigadier.suggestion.SuggestionsBuilder;
import de.z0rdak.yawp.api.core.RegionManager;
import de.z0rdak.yawp.commands.arguments.ArgumentUtil;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.data.region.LevelData;
import de.z0rdak.yawp.util.ChatLinkBuilder;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.SharedSuggestionProvider;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;

import java.util.concurrent.CompletableFuture;

import static de.z0rdak.yawp.api.MessageSender.overLayMessage;
import static de.z0rdak.yawp.util.ChatLinkBuilder.buildRegionInfoLink;

/**
 * Suggests regions that can be assigned as children of the region supplied
 * by a previous command argument.
 *
 * <p>A region is considered a valid child when:</p>
 * <ul>
 *     <li>its area is fully contained within the prospective parent's area</li>
 *     <li>it does not already have a local region as its parent</li>
 *     <li>it resides in the same dimension as the prospective parent</li>
 * </ul>
 *
 * <p>
 * Ownership and permission checks are intentionally not performed by this
 * provider and must be validated during command execution.
 * </p>
 *
 * <p>
 * The provider exists solely to improve command completion and should not
 * be considered an authoritative validation mechanism.
 * </p>
 */
public class ValidChildRegionSuggestionProvider implements SuggestionProvider<CommandSourceStack> {

    @Override
    public CompletableFuture<Suggestions> getSuggestions(CommandContext<CommandSourceStack> ctx, SuggestionsBuilder builder) {
        IMarkableRegion parent = ArgumentUtil.getLocalRegionArgument(ctx);
        var maybeLevelData = RegionManager.get().getLevelRegionData(parent.getDim());
        if (maybeLevelData.isEmpty()) {
            return Suggestions.empty();
        }
        LevelData levelData = maybeLevelData.get();
        var validChildren = levelData.getLocalList().stream()
                .filter(candidate -> isValidChild(parent, candidate))
                .map(IMarkableRegion::getName)
                .toList();
        if (validChildren.isEmpty()) {
            if (ctx.getSource().getPlayer() instanceof ServerPlayer player) {
                overLayMessage(player ,Component.translatableWithFallback("cli.arg.region.add.child.no-valid", "There are no valid child regions for region %s.", ChatLinkBuilder.buildRegionInfoLink(parent)));
            }
            return Suggestions.empty();
        }
        // TODO: permission check here?
        return SharedSuggestionProvider.suggest(validChildren, builder);
    }

    private static boolean isValidChild(IMarkableRegion parent, IMarkableRegion candidate) {
        return parent.getArea().containsOther(candidate.getArea()) && !candidate.hasLocalParent() && !parent.getId().equals(candidate.getId());
    }
}