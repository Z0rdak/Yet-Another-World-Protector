package de.z0rdak.yawp.commands.suggestions;

import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import com.mojang.brigadier.suggestion.SuggestionProvider;
import com.mojang.brigadier.suggestion.Suggestions;
import com.mojang.brigadier.suggestion.SuggestionsBuilder;
import de.z0rdak.yawp.commands.arguments.ArgumentUtil;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.util.ChatLinkBuilder;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.MutableComponent;
import net.minecraft.server.level.ServerPlayer;

import java.util.List;
import java.util.concurrent.CompletableFuture;

import static de.z0rdak.yawp.api.MessageSender.overLayMessage;
import static de.z0rdak.yawp.api.MessageSender.sendCmdFeedback;
import static de.z0rdak.yawp.util.ChatLinkBuilder.buildRegionInfoLink;

public class ExistingFlagsSuggestionProvider implements SuggestionProvider<CommandSourceStack> {
    @Override
    public CompletableFuture<Suggestions> getSuggestions(CommandContext<CommandSourceStack> ctx, SuggestionsBuilder builder) throws CommandSyntaxException {
        var region = ArgumentUtil.getRegionArgument(ctx);
        List<String> flagsInRegion = region.getFlags().flags().stream()
                .map(IFlag::getName)
                .toList();
        if (flagsInRegion.isEmpty()){
            if (ctx.getSource().getPlayer() instanceof ServerPlayer player){
                overLayMessage(player, Component.translatableWithFallback("cli.msg.info.region.flag.no-flags-plain", "No flags defined in region %s!", buildRegionInfoLink(region)));
            }
            MutableComponent hint = Component.translatableWithFallback("cli.msg.info.region.flag.add-hint", "Add flag by clicking: %s", ChatLinkBuilder.buildSuggestAddFlagLink(region));
            sendCmdFeedback(ctx.getSource(), Component.translatableWithFallback("cli.msg.info.region.flag.no-flags", "No flags defined in region %s! %s", buildRegionInfoLink(region), hint));
            return Suggestions.empty();
        }
        return null;
    }
}

