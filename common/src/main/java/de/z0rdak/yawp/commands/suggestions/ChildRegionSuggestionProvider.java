package de.z0rdak.yawp.commands.suggestions;

import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.suggestion.SuggestionProvider;
import com.mojang.brigadier.suggestion.Suggestions;
import com.mojang.brigadier.suggestion.SuggestionsBuilder;
import de.z0rdak.yawp.commands.arguments.ArgumentUtil;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;

import java.util.concurrent.CompletableFuture;

import static de.z0rdak.yawp.api.MessageSender.overLayMessage;
import static de.z0rdak.yawp.util.ChatLinkBuilder.buildRegionInfoLink;

/**

 */
public class ChildRegionSuggestionProvider implements SuggestionProvider<CommandSourceStack> {

    @Override
    public CompletableFuture<Suggestions> getSuggestions(CommandContext<CommandSourceStack> ctx, SuggestionsBuilder builder) {
        IMarkableRegion parent = ArgumentUtil.getLocalRegionArgument(ctx);
        var children = parent.getChildren().values();
        if (children.isEmpty()) {
            if (ctx.getSource().getPlayer() instanceof ServerPlayer player) {
                overLayMessage(player, Component.translatableWithFallback("cli.arg.region.add.child.no-children", "Region %s has no child regions.", buildRegionInfoLink(parent)));
            }
            return Suggestions.empty();
        }
        parent.getChildren().values()
                .stream()
                .map(IMarkableRegion.class::cast)
                .forEach(child -> builder.suggest(child.getName()));
        return builder.buildFuture();
    }
}